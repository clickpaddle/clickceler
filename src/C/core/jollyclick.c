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

//Load rules for Prolog Thread 
static int load_rules_for_thread(intptr_t tid, module_t *out_module) {
    char modname[32];
    char thread_goal[32];
    snprintf(modname, sizeof modname, "rules_%ld", (long)tid);
    snprintf(thread_goal, sizeof thread_goal, "thread_goal_%ld", (long)tid);
    *out_module = PL_new_module(PL_new_atom(modname));

    char filename[64];
    // Naming of rulesets.
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

    // Init Module 
    predicate_t p = PL_predicate(&thread_goal, 1, modname);
    term_t t = PL_new_term_ref(); PL_put_integer(t, (long)tid);
    (void)PL_call_predicate(*out_module, PL_Q_NORMAL, p, t);

    return 1;
}

// Thread 0 to collect events
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

    // assert_json_event/2 s in rules_0.pl ?
    predicate_t p_assert = PL_predicate("assert_json_event", 2, mod);
    if (!p_assert) {
        fprintf(stderr, "[ERR] Event Collector Thread %ld: Can't find predicate 'assert_json_event/2' in ruleset '%s'.\n", logical_id, PL_module_name(mod));
        PL_thread_destroy_engine();
        close(sockfd);
        return NULL;
    }


    printf("[Thread Collect %ld] ready to received JSON events JSON.\n", logical_id);
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
        printf("[Thread Collect %ld] Client is disconnected\n", logical_id);
    else if (nbytes < 0)
        perror("recv [Thread Collecte]");

    close(sockfd);
    PL_thread_destroy_engine();
    return NULL;
}

// Thread to execute the client_1 ruleset rules_1.pl
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
    if (!load_rules_for_thread(logical_id, &mod)) { // load rules_1.pl
        PL_thread_destroy_engine();
        close(sockfd);
        return NULL;
    }

    char buffer[MAX_LINE];
    ssize_t nbytes;

    printf("[Thread  Request  %ld] ready to receive queries Prolog.\n", logical_id);
    while ((nbytes = recv(sockfd, buffer, sizeof(buffer) - 1, 0)) > 0) {
        buffer[nbytes] = '\0';
        printf("[Thread Request %ld] Query received: %s\n", logical_id, buffer);

//--------------------------------------------------------------------------------------------------

char *execute_prolog_query_with_output(const char *query_str, atom_t module) {
    // Step 1: Parse the predicate name from query_str
    char predicate_name[256];
    if (sscanf(query_str, "%255[^ (]", predicate_name) != 1) {
        fprintf(stderr, "[ERROR] Could not parse predicate from: %s\n", query_str);
        return NULL;
    }

    // Step 2: Construct goal: predicate_name(Var)
    functor_t functor = PL_new_functor(PL_new_atom(predicate_name), 1);
    term_t var = PL_new_term_ref();             // This will capture the result
    term_t goal = PL_new_term_ref();            // This holds the full call: functor(var)
    PL_cons_functor(goal, functor, var);        // goal = predicate_name(var)

    // Step 3: Call Prolog
    module_t mod = PL_new_module(module);
    qid_t qid = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, goal, NULL);

    char *output = NULL;

    if (PL_next_solution(qid)) {
        // Extract result
        char *str;
        if (PL_get_chars(var, &str, CVT_WRITE | CVT_ALL | BUF_DISCARDABLE)) {
            output = strdup(str);  // Copy result so it's safe to return
        } else {
            fprintf(stderr, "[ERROR] Could not convert result to string.\n");
        }
    } else {
        // Query failed or exception
        term_t ex = PL_exception(qid);
        if (ex) {
            char *err;
            if (PL_get_chars(ex, &err, CVT_WRITE | CVT_ALL | BUF_DISCARDABLE)) {
                fprintf(stderr, "[ERROR] Exception: %s\n", err);
            }
        } else {
            fprintf(stderr, "[INFO] Query returned no results.\n");
        }
    }

    PL_close_query(qid);
    return output;  // Might be NULL on error
}



char *result = execute_prolog_query_with_output("get_all_my_events(J).", PL_new_atom("rules_1"));
if (result) {
    send(sockfd, result, strlen(result), 0);
    free(result);
} else {
    send(sockfd, "Error: Could not retrieve results.\n", 35, 0);
}




//--------------------------------------------------------------------------------------------------




    }

    if (nbytes == 0)
        printf("[Thread Request %ld] Client is disconnected\n", logical_id);
    else if (nbytes < 0)
        perror("recv [Thread Request]");

    close(sockfd);
    PL_thread_destroy_engine();
    return NULL;
}

// Thread  to run internal rules (client_id 2)
void *severity_updater_thread(void *arg) {
    intptr_t logical_id = (intptr_t)arg; // ID 2 for this thread 

    if (!PL_thread_attach_engine(NULL)) {
        fprintf(stderr, "[ERR] Severitviy Updater Thread %ld: PL_thread_attach_engine failed\n", logical_id);
        return NULL;
    }

    module_t mod;
    if (!load_rules_for_thread(logical_id, &mod)) { // Load rules_2.pl
        PL_thread_destroy_engine();
        return NULL;
    }

    printf("[Thread internal rules %ld] started. Call update_event_severity...\n", logical_id);

    // Check that update_event_severity/0 is defined in dans rules_2.pl
    predicate_t p_update_severity = PL_predicate("update_event_severity", 0, mod);
    if (!p_update_severity) {
        fprintf(stderr, "[ERR] Thread Internal rules %ld: Predicate 'update_event_severity/0' s not in ruleset '%s'.\n", logical_id, PL_module_name(mod));
        PL_thread_destroy_engine();
        return NULL;
    }

    //loop to  Execute internal rules
    while (1) {
        printf("[Thread internal rule %ld]  loop execution update_event_severity...\n", logical_id);
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
            printf("[Thread internal rule %ld] Periodic update_event_severity was successful.\n", logical_id);
        }
        PL_close_query(q_periodic);
        sleep(10); // update every 10 second (ajustez si besoin)
    }

    // Never executed 
    PL_thread_destroy_engine();
    return NULL;
}


// Launch server and listen 
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
    printf("[INFO] Server started and listening on port %d\n", port);
    return server_fd;
}


int main(int argc, char **argv) {
    char *plav[2] = {argv[0], "-q"};
    if (!PL_initialise(2, plav)) {
        fprintf(stderr, "[ERR] Failure to initialize SWI-Prolog\n");
        return 1;
    }

/*
    predicate_t pred = PL_predicate("mutex_create", 1, NULL);
    term_t t0 = PL_new_term_refs(1);
    PL_put_atom_chars(t0, "event_update");

    qid_t q = PL_open_query(NULL, PL_Q_NORMAL, pred, t0);
    int result = PL_next_solution(q);
    PL_close_query(q);

    if (!result) {
        fprintf(stderr, "Failed to create mutex event_update or it already exists.\n");
    } else {
        printf("Mutex event_update created successfully.\n");
    }

*/
    // Launch thread internal rules (ID 2)
    pthread_t severity_pthread_id;
    if (pthread_create(&severity_pthread_id, NULL, severity_updater_thread, (void*)2) != 0) {
        perror("[ERR] pthread_create severity_updater_thread");
        PL_halt(1);
    }
    pthread_detach(severity_pthread_id);


    // Launch server and listen for events (ID logique 0)
    int event_server_fd = setup_and_listen_server(SERVER_PORT_EVENTS);
    if (event_server_fd < 0) { PL_halt(1); }

    // Launch server and listen for external queries (ID  1)
    int query_server_fd = setup_and_listen_server(SERVER_PORT_QUERIES);
    if (query_server_fd < 0) { close(event_server_fd); PL_halt(1); }


    // Loop to accept TCP connections 
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
            targ->thread_logical_id = 0; // ID 0 for event collector 

            pthread_t tid;
            if (pthread_create(&tid, NULL, event_collector_handler, targ) != 0) {
                perror("[ERR] pthread_create event_collector_handler");
                close(new_sock);
                free(targ);
                continue;
            }
            pthread_detach(tid);
            printf("[INFO] New connection for events(Collector #0)\n");
        }

        if (FD_ISSET(query_server_fd, &read_fds)) {
            struct sockaddr_in client_addr;
            socklen_t client_len = sizeof(client_addr);
            int new_sock = accept(query_server_fd, (struct sockaddr *)&client_addr, &client_len);
            if (new_sock < 0) { perror("[ERR] accept on query port"); continue; }

            thread_arg_t *targ = malloc(sizeof(thread_arg_t));
            if (!targ) { fprintf(stderr, "[ERR] malloc\n"); close(new_sock); continue; }

            targ->sockfd = new_sock;
            targ->thread_logical_id = 1; // ID 1 for the query collector 

            pthread_t tid;
            if (pthread_create(&tid, NULL, query_executor_handler, targ) != 0) {
                perror("[ERR] pthread_create query_executor_handler");
                close(new_sock);
                free(targ);
                continue;
            }
            pthread_detach(tid);
            printf("[INFO] New connection for queries (Collector 1)\n");
        }
    }

    close(event_server_fd);
    close(query_server_fd);
    PL_halt(0);
    return 0;
}
