#define _DEFAULT_SOURCE
#include <SWI-Prolog.h>
#include <errno.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <global.h>

extern int keep_running;


//------------------------------------------------------------------------


char *execute_prolog_query_with_output(const char *query_str, atom_t mod_atom) {
    fprintf(stderr, "[DEBUG] [Thread %ld] Entering execute_prolog_query_with_output for query: %s\n",
            (long)pthread_self(), query_str);

    char predicate_name[256];
    char arg1[256];

    // Parse: pred("arg")
    if (sscanf(query_str, "%255[^ (](%255[^, )]", predicate_name, arg1) != 2) {
        fprintf(stderr, "[ERROR] Failed to parse predicate and arg from: %s\n", query_str);
        return NULL;
    }

    fprintf(stderr, "[DEBUG] Parsed predicate: %s, arg1: %s\n", predicate_name, arg1);

    // Récupérer le nom du module en chaîne C depuis atom_t
    const char *module_name = PL_atom_chars(mod_atom);
    if (!module_name) {
        fprintf(stderr, "[ERROR] Could not convert module atom to string.\n");
        return NULL;
    }

    // Créer la prédicat avec le nom en chaîne C et le nom du module (chaîne C)
    predicate_t pred = PL_predicate(predicate_name, 2, module_name);
    if (!pred) {
        fprintf(stderr, "[ERROR] Could not find predicate %s/2 in module %s\n", predicate_name, module_name);
        return NULL;
    }

    term_t args = PL_new_term_refs(2);  // 2 arguments

    // args[0] = input argument (arg1), args[1] = output argument
    // Ici on suppose arg1 est un atome ou string à passer tel quel.
    PL_put_atom_chars(args, arg1);

    qid_t qid = PL_open_query(mod_atom, PL_Q_CATCH_EXCEPTION, pred, args);

    char *results = NULL;
    size_t total_len = 0;

    while (PL_next_solution(qid)) {
        char *buf = NULL;

        if (PL_get_chars(args + 1, &buf, CVT_WRITE | BUF_DISCARDABLE)) {
            size_t len = strlen(buf);

            // Reallocation du buffer résultat
            char *new_results = realloc(results, total_len + len + 2); // +2 pour '\n' + '\0'
            if (!new_results) {
                fprintf(stderr, "[ERROR] Memory allocation failed.\n");
                free(results);
                PL_close_query(qid);
                return NULL;
            }

            results = new_results;
            memcpy(results + total_len, buf, len);
            total_len += len;
            results[total_len++] = '\n';
            results[total_len] = '\0';
        } else {
            fprintf(stderr, "[WARN] Could not convert output to string.\n");
        }
    }

    PL_close_query(qid);

    if (!results) {
        fprintf(stderr, "[INFO] No Prolog solution found.\n");
        return strdup("");  // return empty string instead of NULL
    }

    return results;
}

//------------------------------------------------------------------------------------------    */
// Assuming MAX_LINE is defined elsewhere, e.g., #define MAX_LINE 4096

// Corrected collector_handler function
static void *collector_handler(PL_engine_t logical_id, int sockfd, atom_t mod_atom, module_t mod) {
    fprintf(stderr, "[DEBUG] [Collector %ld] Entering collector_handler for socket %d\n", (long)logical_id, sockfd);

    ssize_t nbytes;
    char buffer[MAX_LINE];

    printf("[Collector] ready to receive JSON EVENT or QUERY.\n");

    while ((nbytes = recv(sockfd, buffer, sizeof(buffer) - 1, 0)) > 0) {
        buffer[nbytes] = '\0';
        fprintf(stderr, "[Collector] Received %zd bytes: %s\n", nbytes, buffer);

        if (strncmp(buffer, "EVENT ", 6) == 0) {
            char *event_json = buffer + 6;
            fprintf(stderr, "[DEBUG] [Collector %ld] Identified as EVENT. JSON: %s\n", (long)logical_id, event_json);

        term_t t = PL_new_term_ref();
        PL_put_atom(t, mod_atom);  // ← convert atom_t into a term_t
        
        char *mod_name = NULL;
        if (!PL_get_atom_chars(t, &mod_name)) {
            fprintf(stderr, "Could not extract mod name from atom\n");
        } else {
            fprintf(stderr, "Module name: %s\n", mod_name);
        }
            // Get predicate handle for collect_events/1 in the specified module
            predicate_t pred = PL_predicate("collect_events", 1, mod_name);
            if (!pred) {
                fprintf(stderr, "[ERR] [Collector %ld] Can't find predicate 'collect_events/1' in module '%s'.\n", (long)logical_id, mod_name);
                continue;
            }

            // Create a Prolog term reference for the parsed JSON.
            // This term will be the single argument for collect_events/1.
 // Create Prolog term for JSON string (as atom)
            term_t arg = PL_new_term_ref();
            if (!PL_put_atom_chars(arg, event_json)) {
                fprintf(stderr, "[ERR] [Collector %ld] Failed to create Prolog atom from JSON string\n", (long)logical_id);
                continue;
            }

            // Call collect_events(JSONAtom) with exception catching
            qid_t q = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, pred, arg);
            int result = PL_next_solution(q);
            if (!result) {
                term_t ex = PL_exception(q);
                if (ex) {
                    char *err_msg = NULL;
                    if (PL_get_chars(ex, &err_msg, CVT_WRITE | BUF_DISCARDABLE)) {
                        fprintf(stderr, "[ERR] [Collector %ld] Prolog exception: %s\n", (long)logical_id, err_msg);
                    } else {
                        fprintf(stderr, "[ERR] [Collector %ld] Prolog exception (could not extract message)\n", (long)logical_id);
                    }
                } else {
                    fprintf(stderr, "[ERR] [Collector %ld] collect_events/1 failed with no exception.\n", (long)logical_id);
                }
            } else {
                fprintf(stderr, "[DEBUG] [Collector %ld] collect_events/1 succeeded.\n", (long)logical_id);
            }

            PL_close_query(q);


   } else if (strncmp(buffer, "QUERY ", 6) == 0) {
      char *query_str = buffer + 6;

      printf("[Collector %ld] Received QUERY: %s\n", logical_id,
             query_str);
      fprintf(stderr, "[DEBUG] [Collector %ld] Identified as QUERY. Query string: %s\n", (long) logical_id, query_str);
      atom_t mod_atom = PL_new_atom("collector");
      char *result = execute_prolog_query_with_output(query_str, mod_atom);
      if (result) {
        send(sockfd, result, strlen(result), 0);
        fprintf(stderr, "[DEBUG] [Collector %ld] Sent query result: %s\n", logical_id, result);
        free(result);
      } else {
        send(sockfd, "Error: Could not retrieve results.\n", 35, 0);
        fprintf(stderr, "[ERROR] [Collector %ld] Failed to get query result.\n", logical_id);
      }
    } else {
      fprintf(stderr, "[Collector] Neither EVENT, nor QUERY received.\n " "%s\n", buffer);
    }
  }
  fprintf(stderr, "[DEBUG] [Collector] Client disconnected from socket %d.\n",  sockfd);
  return NULL; // Ensure a return value for void*
}

//------------------------------------------------------------------------------------------

static void *per_client_thread(void *arg) {
  thread_arg_t *targ = (thread_arg_t *)arg;
  fprintf(stderr, "[DEBUG] [Collector %ld] Starting per_client_thread for socket %d\n", targ->thread_logical_id, targ->sockfd);
  PL_engine_t collector_engine=PL_thread_attach_engine(NULL) ;
  if (collector_engine == NULL)  {
    fprintf(stderr, "[ERR] [Collector %ld]: PL_thread_attach_engine failed\n", targ->thread_logical_id);
    close(targ->sockfd);
    free(targ);
    return NULL;
  }
  fprintf(stderr, "[DEBUG] [Collector %ld] Prolog engine attached.\n", targ->thread_logical_id);
  const char *modname = "collector";
  atom_t modname_atom = PL_new_atom_nchars(strlen(modname), modname);
  module_t mod = PL_new_module(modname_atom);

if (load_rules_for_thread(collector_engine, mod, modname_atom) != 0) {
    fprintf(stderr, "[Collector] Failed to load rules for thread\n");
    PL_thread_destroy_engine();
    close(targ->sockfd);
    free(targ);
    return NULL;
}
  fprintf(stderr, "[Collector] Rules loaded.\n");
  collector_handler(collector_engine,targ->sockfd, modname_atom,mod);
  PL_unregister_atom(modname_atom);
  fprintf(stderr, "[Collector] Collector handler finished.\n" );

  close(targ->sockfd);
  PL_thread_destroy_engine();
  fprintf(stderr, "[DEBUG] [Collector %ld] Exiting per_client_thread.\n", targ->thread_logical_id);
  free(targ);
  return NULL;
}


//------------------------------------------------------------------------------------------
void *collector_handler_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg; // ID 1 for this thread
  fprintf(stderr, "[DEBUG] [Collector Handler %ld] Entering collector_handler_thread\n", logical_id);

  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  uint16_t port = SERVER_PORT;
  addr.sin_port = htons(port);
  int server_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (server_fd < 0) {
    perror("[ERR] socket creation failed");
    PL_halt(1);
  }
  fprintf(stderr, "[DEBUG] [Collector Handler %ld] Socket created.\n", logical_id);


  if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    perror("[ERR] bind");
    close(server_fd);
    return (void *)-1; // Cast to void*
  }
  fprintf(stderr, "[DEBUG] [Collector Handler %ld] Socket bound to port %d.\n", logical_id, port);

  if (listen(server_fd, MAX_CLIENTS) < 0) {
    perror("[ERR] listen");
    close(server_fd);
    return (void *)-1; // Cast to void*
  }
  printf("[INFO] [Collector Handler %ld] Server started and listening on port %d\n", logical_id, port);


  while(1) {
    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(server_fd, &read_fds);
    int max_fd = server_fd;
    int new_sock;
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    fprintf(stderr, "[DEBUG] [Collector Handler %ld] Waiting for new connections...\n", logical_id);
    int activity = select(max_fd + 1, &read_fds, NULL, NULL, NULL);
    if (activity < 0 && errno != EINTR) {
      perror("[ERR] select");
      break;
    }
    if (FD_ISSET(server_fd, &read_fds)) {
            new_sock = accept(server_fd, (struct sockaddr *)&client_addr, &client_len);
            if (new_sock < 0) {
                perror("[ERR] accept on event port");
                break; 
            }
            
            // Allocate the structure to pass arguments to the new thread
            thread_arg_t *targ = malloc(sizeof(thread_arg_t));
            if (!targ) {
                perror("[ERR] malloc for thread_arg_t");
                close(new_sock);
                continue;
            }
            
            targ->sockfd = new_sock;
            
            targ->thread_logical_id = (uintptr_t)pthread_self(); // Stores the casted numerical ID
            // ---------------------------------------------------------------------------------

            fprintf(stderr, "[DEBUG] [Collector Handler %zu] Accepted new connection on socket %d.\n",
                    targ->thread_logical_id, new_sock); // Uses the numerical ID of the client thread

            pthread_t tid;
            if (pthread_create(&tid, NULL, per_client_thread, targ) != 0) {
                fprintf(stderr, "[DEBUG] [Collector Handler %zu] failed to create thread.\n", targ->thread_logical_id);
                free(targ); // Free memory in case of thread creation failure
                close(new_sock);
                continue;
            }
            // Detach the thread if you don't plan to join it
            // or store `tid` for a later `pthread_join`.
            fprintf(stderr, "[DEBUG] [Collector Handler %zu] created thread.\n", targ->thread_logical_id);
            pthread_detach(tid); 
    }
  }
  fprintf(stderr, "[DEBUG] [Collector Handler %ld] Exiting collector_handler_thread.\n", logical_id);
  close(server_fd);
  return NULL;
}
//------------------------------------------------------------------------------------------
                  
