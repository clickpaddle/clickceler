#define _DEFAULT_SOURCE
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <SWI-Prolog.h>

#define SERVER_PORT_EVENTS 65432
#define SERVER_PORT_QUERIES 65433 // Nouveau port pour le client externe (requêtes)
#define MAX_CLIENTS 10
#define MAX_LINE 8192

typedef struct {
    int sockfd;
    intptr_t thread_logical_id; // ID logique (0 pour event, 1 pour query, 2 pour severity)
} thread_arg_t;

// Fonction utilitaire pour charger les règles Prolog pour un thread donné
static int load_rules_for_thread(intptr_t tid, module_t *out_module) {
    char modname[32];
    snprintf(modname, sizeof modname, "thread_%ld", (long)tid);
    *out_module = PL_new_module(PL_new_atom(modname));

    char filename[64];
    // Les fichiers de règles sont maintenant nommés directement par leur ID logique
    snprintf(filename, sizeof filename, "../rules/rules_%ld.pl", (long)tid);

    char goal_buf[256];
    snprintf(goal_buf, sizeof goal_buf,
             "load_files('%s', [if(true), module(%s)])",
             filename, modname);

    term_t goal = PL_new_term_ref();
    if (!PL_chars_to_term(goal_buf, goal) || !PL_call(goal, NULL)) {
        fprintf(stderr, "[ERR] Thread %ld: échec chargement %s\n", (long)tid, filename);
        return 0;
    }

    // Appeler thread_goal(tid) si défini (bonne pratique pour l'initialisation du module)
    predicate_t p = PL_predicate("thread_goal", 1, modname);
    term_t t = PL_new_term_ref(); PL_put_integer(t, (long)tid);
    (void)PL_call_predicate(*out_module, PL_Q_NORMAL, p, t);

    return 1;
}

// Thread pour la collecte des événements (client_id 0)
void *event_collector_handler(void *arg) {
    thread_arg_t *targ = (thread_arg_t *)arg;
    int sockfd = targ->sockfd;
    intptr_t logical_id = targ->thread_logical_id;
    free(targ);

    if (!PL_thread_attach_engine(NULL)) {
        fprintf(stderr, "[ERR] Event Collector Thread %ld: PL_thread_attach_engine failed\n", logical_id);
        close(sockfd);
        return NULL;
    }

    module_t mod;
    if (!load_rules_for_thread(logical_id, &mod)) { // Charge rules_0.pl
        PL_thread_destroy_engine();
        close(sockfd);
        return NULL;
    }

    char buffer[MAX_LINE];
    ssize_t nbytes;

    // Assurez-vous que assert_json_event/2 est défini dans rules_0.pl
    predicate_t p_assert = PL_predicate("assert_json_event", 2, mod);
    if (!p_assert) {
        fprintf(stderr, "[ERR] Event Collector Thread %ld: Prédicat 'assert_json_event/2' introuvable dans le module '%s'.\n", logical_id, PL_module_name(mod));
        PL_thread_destroy_engine();
        close(sockfd);
        return NULL;
    }


    printf("[Thread Collecte %ld] Prêt à recevoir des événements JSON.\n", logical_id);
    while ((nbytes = recv(sockfd, buffer, sizeof(buffer) - 1, 0)) > 0) {
        buffer[nbytes] = '\0';
        printf("[Thread Collecte %ld] Reçu JSON: %s\n", logical_id, buffer);

        term_t args = PL_new_term_refs(2);
        PL_put_integer(args, (long)logical_id);
        PL_put_atom_chars(args + 1, buffer);

        qid_t q = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, p_assert, args);
        if (!PL_next_solution(q)) {
            term_t ex = PL_exception(q);
            if (ex) {
                char *ex_str = NULL;
                if (PL_get_chars(ex, &ex_str, CVT_ALL)) {
                    fprintf(stderr,"[Thread Collecte %ld] Prolog Error: %s\n", logical_id, ex_str);
                } else {
                    fprintf(stderr,"[Thread Collecte %ld] Prolog Error (non-stringifiable exception)\n", logical_id);
                }
            }
        }
        PL_close_query(q);
    }

    if (nbytes == 0)
        printf("[Thread Collecte %ld] Client déconnecté\n", logical_id);
    else if (nbytes < 0)
        perror("recv [Thread Collecte]");

    close(sockfd);
    PL_thread_destroy_engine();
    return NULL;
}

// Thread pour l'exécution des requêtes par un client externe (client_id 1)
void *query_executor_handler(void *arg) {
    thread_arg_t *targ = (thread_arg_t *)arg;
    int sockfd = targ->sockfd;
    intptr_t logical_id = targ->thread_logical_id;
    free(targ);

    if (!PL_thread_attach_engine(NULL)) {
        fprintf(stderr, "[ERR] Query Executor Thread %ld: PL_thread_attach_engine failed\n", logical_id);
        close(sockfd);
        return NULL;
    }

    module_t mod;
    if (!load_rules_for_thread(logical_id, &mod)) { // Charge rules_1.pl
        PL_thread_destroy_engine();
        close(sockfd);
        return NULL;
    }

    char buffer[MAX_LINE];
    ssize_t nbytes;

    printf("[Thread Requêtes %ld] Prêt à recevoir des requêtes Prolog.\n", logical_id);
    while ((nbytes = recv(sockfd, buffer, sizeof(buffer) - 1, 0)) > 0) {
        buffer[nbytes] = '\0';
        printf("[Thread Requêtes %ld] Reçu requête: %s\n", logical_id, buffer);

        term_t goal = PL_new_term_ref();
        // Convertir la chaîne reçue en terme Prolog (but)
        if (!PL_chars_to_term(buffer, goal)) {
            fprintf(stderr, "[Thread Requêtes %ld] Erreur: impossible de parser la requête '%s'.\n", logical_id, buffer);
            send(sockfd, "Error: Invalid query format.\n", 28, 0);
            continue;
        }

        // Exécuter la requête
        qid_t q = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, goal, NULL);
        if (PL_next_solution(q)) {
            // Si la requête réussit, envoyer une confirmation ou le résultat
            send(sockfd, "Query successful.\n", 18, 0);
        } else {
            term_t ex = PL_exception(q);
            if (ex) {
                char *ex_str = NULL;
                if (PL_get_chars(ex, &ex_str, CVT_ALL)) {
                    fprintf(stderr,"[Thread Requêtes %ld] Prolog Error: %s\n", logical_id, ex_str);
                    char response_buf[MAX_LINE];
                    snprintf(response_buf, sizeof(response_buf), "Prolog Error: %s\n", ex_str);
                    send(sockfd, response_buf, strlen(response_buf), 0);
                } else {
                    fprintf(stderr,"[Thread Requêtes %ld] Prolog Error (non-stringifiable exception)\n", logical_id);
                    send(sockfd, "Prolog Error (non-stringifiable exception)\n", 44, 0);
                }
            } else {
                fprintf(stderr,"[Thread Requêtes %ld] Query failed (no exception caught).\n", logical_id);
                send(sockfd, "Query failed.\n", 14, 0);
            }
        }
        PL_close_query(q);
    }

    if (nbytes == 0)
        printf("[Thread Requêtes %ld] Client déconnecté\n", logical_id);
    else if (nbytes < 0)
        perror("recv [Thread Requêtes]");

    close(sockfd);
    PL_thread_destroy_engine();
    return NULL;
}

// Thread pour la mise à jour de la sévérité (client_id 2)
void *severity_updater_thread(void *arg) {
    intptr_t logical_id = (intptr_t)arg; // ID logique 2 pour ce thread

    if (!PL_thread_attach_engine(NULL)) {
        fprintf(stderr, "[ERR] Severity Updater Thread %ld: PL_thread_attach_engine failed\n", logical_id);
        return NULL;
    }

    module_t mod;
    if (!load_rules_for_thread(logical_id, &mod)) { // Charge rules_2.pl
        PL_thread_destroy_engine();
        return NULL;
    }

    printf("[Thread Sévérité %ld] Démarré. Appel de update_event_severity...\n", logical_id);

    // Assurez-vous que update_event_severity/0 est défini dans rules_2.pl
    predicate_t p_update_severity = PL_predicate("update_event_severity", 0, mod);
    if (!p_update_severity) {
        fprintf(stderr, "[ERR] Thread Sévérité %ld: Prédicat 'update_event_severity/0' introuvable dans le module '%s'.\n", logical_id, PL_module_name(mod));
        PL_thread_destroy_engine();
        return NULL;
    }

    // Boucle pour l'exécution périodique de la règle de sévérité
    while (1) {
        printf("[Thread Sévérité %ld] Exécution périodique de update_event_severity...\n", logical_id);
        qid_t q_periodic = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, p_update_severity, 0);
        if (!PL_next_solution(q_periodic)) {
            term_t ex = PL_exception(q_periodic);
            if (ex) {
                char *ex_str = NULL;
                if (PL_get_chars(ex, &ex_str, CVT_ALL)) {
                    fprintf(stderr,"[Thread Sévérité %ld] Prolog Error during periodic update_event_severity: %s\n", logical_id, ex_str);
                } else {
                    fprintf(stderr,"[Thread Sévérité %ld] Prolog Error (non-stringifiable exception) during periodic update_event_severity\n", logical_id);
                }
            } else {
                fprintf(stderr,"[Thread Sévérité %ld] Periodic update_event_severity failed (no exception caught).\n", logical_id);
            }
        } else {
            printf("[Thread Sévérité %ld] Periodic update_event_severity a été exécuté avec succès.\n", logical_id);
        }
        PL_close_query(q_periodic);
        sleep(10); // Mettre à jour toutes les 10 secondes (ajustez si besoin)
    }

    // Ce code ne sera jamais atteint si la boucle while(1) est active
    PL_thread_destroy_engine();
    return NULL;
}


// Fonction utilitaire pour configurer et lancer un serveur d'écoute
int setup_and_listen_server(int port) {
    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) { perror("[ERR] socket"); return -1; }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(port);

    if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        perror("[ERR] bind"); close(server_fd); return -1;
    }

    if (listen(server_fd, MAX_CLIENTS) < 0) {
        perror("[ERR] listen"); close(server_fd); return -1;
    }
    printf("[INFO] Serveur d'écoute démarré sur le port %d\n", port);
    return server_fd;
}


int main(int argc, char **argv) {
    char *plav[2] = {argv[0], "-q"};
    if (!PL_initialise(2, plav)) {
        fprintf(stderr, "[ERR] Echec initialisation SWI-Prolog\n");
        return 1;
    }

    // Lancer le thread de mise à jour de la sévérité (ID logique 2)
    pthread_t severity_pthread_id;
    if (pthread_create(&severity_pthread_id, NULL, severity_updater_thread, (void*)2) != 0) {
        perror("[ERR] pthread_create severity_updater_thread");
        PL_halt(1);
    }
    pthread_detach(severity_pthread_id);


    // Démarrer le serveur d'écoute pour la collecte des événements (ID logique 0)
    int event_server_fd = setup_and_listen_server(SERVER_PORT_EVENTS);
    if (event_server_fd < 0) { PL_halt(1); }

    // Démarrer le serveur d'écoute pour les requêtes externes (ID logique 1)
    int query_server_fd = setup_and_listen_server(SERVER_PORT_QUERIES);
    if (query_server_fd < 0) { close(event_server_fd); PL_halt(1); }


    // Boucle principale pour accepter les connexions
    while (1) {
        fd_set read_fds;
        FD_ZERO(&read_fds);
        FD_SET(event_server_fd, &read_fds);
        FD_SET(query_server_fd, &read_fds);

        int max_fd = (event_server_fd > query_server_fd) ? event_server_fd : query_server_fd;

        int activity = select(max_fd + 1, &read_fds, NULL, NULL, NULL);

        if (activity < 0 && errno != EINTR) {
            perror("[ERR] select");
            break;
        }

        if (FD_ISSET(event_server_fd, &read_fds)) {
            struct sockaddr_in client_addr;
            socklen_t client_len = sizeof(client_addr);
            int new_sock = accept(event_server_fd, (struct sockaddr *)&client_addr, &client_len);
            if (new_sock < 0) { perror("[ERR] accept on event port"); continue; }

            thread_arg_t *targ = malloc(sizeof(thread_arg_t));
            if (!targ) { fprintf(stderr, "[ERR] malloc\n"); close(new_sock); continue; }

            targ->sockfd = new_sock;
            targ->thread_logical_id = 0; // ID logique 0 pour le collecteur d'événements

            pthread_t tid;
            if (pthread_create(&tid, NULL, event_collector_handler, targ) != 0) {
                perror("[ERR] pthread_create event_collector_handler");
                close(new_sock);
                free(targ);
                continue;
            }
            pthread_detach(tid);
            printf("[INFO] Nouvelle connexion sur le port événements (Collecteur #0)\n");
        }

        if (FD_ISSET(query_server_fd, &read_fds)) {
            struct sockaddr_in client_addr;
            socklen_t client_len = sizeof(client_addr);
            int new_sock = accept(query_server_fd, (struct sockaddr *)&client_addr, &client_len);
            if (new_sock < 0) { perror("[ERR] accept on query port"); continue; }

            thread_arg_t *targ = malloc(sizeof(thread_arg_t));
            if (!targ) { fprintf(stderr, "[ERR] malloc\n"); close(new_sock); continue; }

            targ->sockfd = new_sock;
            targ->thread_logical_id = 1; // ID logique 1 pour le client de requêtes

            pthread_t tid;
            if (pthread_create(&tid, NULL, query_executor_handler, targ) != 0) {
                perror("[ERR] pthread_create query_executor_handler");
                close(new_sock);
                free(targ);
                continue;
            }
            pthread_detach(tid);
            printf("[INFO] Nouvelle connexion sur le port requêtes (Exécuteur #1)\n");
        }
    }

    close(event_server_fd);
    close(query_server_fd);
    PL_halt(0);
    return 0;
}
