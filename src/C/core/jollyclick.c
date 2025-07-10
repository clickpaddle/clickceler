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
      
#define SERVER_PORT 65432
#define MAX_CLIENTS 10
#define MAX_LINE 8192

#define THREAD_ID_COLLECTOR (void *)1
#define THREAD_ID_SEVERITY  (void *)2

typedef struct {
  int sockfd;
  intptr_t thread_logical_id; // ID logique (0 pour event, 1 pour query, 2 pour
                              // severity)
} thread_arg_t;
//------------------------------------------------------------------------------------------    
volatile int keep_running = 1; // Global flag

void handle_sigint(int sig) {
    keep_running = 0;
    // Potentiellement débloquer des appels bloquants (select, accept, sleep)
}
//------------------------------------------------------------------------------------------    
// Load rules for Prolog Thread
static int load_rules_for_thread(intptr_t tid, module_t *out_module) {
  fprintf(stderr, "[DEBUG] [Thread %ld] Entering load_rules_for_thread\n", (long)tid);
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
           "load_files('%s', [if(true), module(%s)])", filename, modname);
  
  fprintf(stderr, "[DEBUG] [Thread %ld] Loading rules %s\n", tid, modname);
  term_t goal = PL_new_term_ref();
  if (!PL_chars_to_term(goal_buf, goal) || !PL_call(goal, NULL)) {
    fprintf(stderr, "[ERR] [Thread %ld]: Loading rules failure %s\n", (long)tid,
            filename); 
    return 0;
  } 
    
  // Init Module
  predicate_t p = PL_predicate(&thread_goal, 1, modname);
  term_t t = PL_new_term_ref();
  PL_put_integer(t, (long)tid);
  (void)PL_call_predicate(*out_module, PL_Q_NORMAL, p, t);
  fprintf(stderr, "[DEBUG] [Thread %ld] Exiting load_rules_for_thread successfully\n", (long)tid);
  return 1;
}   
  
//------------------------------------------------------------------------------------------    

// Thread  to run internal rules (client_id 2)
void *severity_updater_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg; // ID 2 for this thread
  fprintf(stderr, "[DEBUG] [Thread Severity %ld] Entering severity_updater_thread\n", logical_id);

  if (!PL_thread_attach_engine(NULL)) {
    fprintf( stderr, "[ERR] [Thread Severity %ld]: PL_thread_attach_engine failed\n", logical_id);
    return NULL;
  }
  fprintf(stderr, "[DEBUG] [Thread Severity %ld] Prolog engine attached.\n", logical_id);

  module_t mod;
  if (!load_rules_for_thread(logical_id, &mod)) { // Load rules_2.pl
    if (!mod) { fprintf(stderr,"[ERROR] [Thread Severity %ld] Module context is NULL -- Prolog ruleset not loaded properly.\n", logical_id);}
    PL_thread_destroy_engine();
    return NULL;
  }
  fprintf(stderr, "[DEBUG] [Thread Severity %ld] Rules loaded.\n", logical_id);

  printf("[Thread internal rules %ld] started. Call update_event_severity...\n",
         logical_id);

  // Check that update_event_severity/0 is defined in dans rules_2.pl
  predicate_t p_update_severity = PL_predicate("update_event_severity", 0, mod);
  if (!p_update_severity) {
    fprintf(stderr,
            "[ERR] [Thread Internal rules %ld]: Predicate "
            "'update_event_severity/0' s not in ruleset '%s'.\n",
            logical_id, PL_module_name(mod));
    PL_thread_destroy_engine();
    return NULL;
   }
   fprintf(stderr, "[DEBUG] [Thread Severity %ld] Predicate update_event_severity/0 found.\n", logical_id);

   while (1) {
        printf("[Thread internal rule %ld]  loop execution update_event_severity...\n", logical_id);
        qid_t q_periodic = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, p_update_severity, 0);
        if (!PL_next_solution(q_periodic)) {
            term_t ex = PL_exception(q_periodic);
            if (ex) {
                char *ex_str = NULL;
                if (PL_get_chars(ex, &ex_str, CVT_ALL)) {
                    fprintf(stderr,"[Thread Severity %ld] Prolog Error during periodic update_event_severity: %s\n", logical_id, ex_str);
                } else {
                    fprintf(stderr,"[Thread Severity %ld] Prolog Error (non-stringifiable exception) during periodic update_event_severity\n", logical_id);
                }
            } else {
                fprintf(stderr,"[Thread Severity %ld] Periodic update_event_severity failed (no exception caught).\n", logical_id);
            }
        } else {
            printf("[Thread internal rule %ld] Periodic update_event_severity was successful.\n", logical_id);
        }
        PL_close_query(q_periodic);
        fprintf(stderr, "[DEBUG] [Thread Severity %ld] Query for update_event_severity closed. Sleeping for 10 seconds.\n", logical_id);
        sleep(10); // update every 10 second (ajustez si besoin)
    }
    fprintf(stderr, "[DEBUG] [Thread Severity %ld] Exiting severity_updater_thread\n", logical_id);
}

//------------------------------------------------------------------------------------------    

char *execute_prolog_query_with_output(const char *query_str, atom_t mod) {
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

    predicate_t pred = PL_predicate(predicate_name, 2, NULL);  // pred/2
    term_t args = PL_new_term_refs(2);  // [I, J]

    // Handle arg1 (I), assumed to be a quoted string or atom
    PL_put_atom_chars(args, arg1);  // args[0] = I
    // args[1] = J (output)

    qid_t qid = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, pred, args);

    char *results = NULL;
    size_t total_len = 0;

    while (PL_next_solution(qid)) {
        char *buf = NULL;

        if (PL_get_chars(args + 1, &buf, CVT_WRITE | BUF_DISCARDABLE)) {
            size_t len = strlen(buf);

            // Reallocate buffer for result
            char *new_results = realloc(results, total_len + len + 2); // +2 for '\n' + '\0'
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

void *collector_handler(long int logical_id,int sockfd,module_t mod){
  fprintf(stderr, "[DEBUG] [Thread Collect %ld] Entering collector_handler for socket %d\n", logical_id, sockfd);
  ssize_t nbytes;
  char buffer[MAX_LINE];
  printf("[Thread Collect %ld] ready to receive JSON EVENT or QUERY.\n", logical_id);
  while ((nbytes = recv(sockfd, buffer, sizeof(buffer) - 1, 0)) > 0) {
    buffer[nbytes] = '\0';
    fprintf(stderr, "[DEBUG] [Thread Collect %ld] Received %zd bytes: %s\n", logical_id, nbytes, buffer);
    if (strncmp(buffer, "EVENT ", 6) == 0) {
      char *event_json = buffer + 6;
      fprintf(stderr, "[DEBUG] [Thread Collect %ld] Identified as EVENT. JSON: %s\n", logical_id, event_json);
      predicate_t p_assert = PL_predicate("assert_json_event", 2, mod); // This predicate should be assert_json_event/2 if it takes two arguments
      if (!p_assert) {
        fprintf(stderr, "[ERR] [Thread Collector %ld]: Can't find predicate " "'assert_json_event/2' in ruleset '%s'.\n", // Corrected predicate arity in error message
                logical_id, PL_module_name(mod));
        continue; // Skip to next message if predicate not found
      }
      printf("[Thread Collect %ld] Ready to receive EVENT: %s\n", logical_id, event_json);
      term_t args = PL_new_term_refs(2);
      PL_put_integer(args, (long)logical_id);
      PL_put_atom_chars(args + 1, event_json);
      fprintf(stderr, "[DEBUG] [Thread Collect %ld] Calling assert_json_event/2 with logical_id and event_json.\n", logical_id);

      qid_t q = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, p_assert, args);
      if (!PL_next_solution(q)) {
        term_t ex = PL_exception(q);
        if (ex) {
          char *ex_str = NULL;
          if (PL_get_chars(ex, &ex_str, CVT_ALL)) {
            fprintf(stderr, "[Thread Collecte %ld] Prolog Error during assert_json_event: %s\n",
                    logical_id, ex_str);
          } else {
            fprintf(stderr, "[Thread Collecte %ld] Prolog Error (non-stringifiable " "exception) during assert_json_event\n", logical_id);
          }
        } else {
          fprintf(stderr, "[Thread Collecte %ld] assert_json_event failed (no exception caught).\n", logical_id);
        }
      } else {
        fprintf(stderr, "[DEBUG] [Thread Collect %ld] assert_json_event successful.\n", logical_id);
      }
      PL_close_query(q);

   } else if (strncmp(buffer, "QUERY ", 6) == 0) {
      char *query_str = buffer + 6;

      printf("[Thread Collecte %ld] Received QUERY: %s\n", logical_id,
             query_str);
      fprintf(stderr, "[DEBUG] [Thread Collect %ld] Identified as QUERY. Query string: %s\n", (long) logical_id, query_str);
      atom_t mod_atom = PL_new_atom("rule_1");
      char *result = execute_prolog_query_with_output(query_str, mod_atom);
      if (result) {
        send(sockfd, result, strlen(result), 0);
        fprintf(stderr, "[DEBUG] [Thread Collect %ld] Sent query result: %s\n", logical_id, result);
        free(result);
      } else {
        send(sockfd, "Error: Could not retrieve results.\n", 35, 0);
        fprintf(stderr, "[ERROR] [Thread Collect %ld] Failed to get query result.\n", logical_id);
      }
    } else {
      fprintf(stderr,
              "[Thread Collecte %ld] Reçu message inconnu (ni EVENT ni QUERY): "
              "%s\n",
              logical_id, buffer);
    }
  }
  fprintf(stderr, "[DEBUG] [Thread Collect %ld] Client disconnected from socket %d.\n", logical_id, sockfd);
  return NULL; // Ensure a return value for void*
}

//------------------------------------------------------------------------------------------    
void *per_client_thread(void *arg) {
  thread_arg_t *targ = (thread_arg_t *)arg;
  fprintf(stderr, "[DEBUG] [Thread Per-Client %ld] Starting per_client_thread for socket %d\n", targ->thread_logical_id, targ->sockfd);


  if (!PL_thread_attach_engine(NULL)) {
    fprintf(stderr, "[ERR] [Thread Collector %ld]: PL_thread_attach_engine failed\n", targ->thread_logical_id);
    close(targ->sockfd);
    free(targ);
    return NULL;
  }
  fprintf(stderr, "[DEBUG] [Thread Per-Client %ld] Prolog engine attached.\n", targ->thread_logical_id);


  module_t mod;
  if (!load_rules_for_thread(targ->thread_logical_id, &mod)) {
    fprintf(stderr, "[ERR] [Thread Per-Client %ld] Failed to load rules for thread %ld\n", targ->thread_logical_id, targ->thread_logical_id);
    PL_thread_destroy_engine();
    close(targ->sockfd);
    free(targ);
    return NULL;
  }
  fprintf(stderr, "[DEBUG] [Thread Per-Client %ld] Rules loaded.\n", targ->thread_logical_id);

  collector_handler(targ->thread_logical_id, targ->sockfd, mod);
  fprintf(stderr, "[DEBUG] [Thread Per-Client %ld] Collector handler finished.\n", targ->thread_logical_id);

  close(targ->sockfd);
  PL_thread_destroy_engine();
  fprintf(stderr, "[DEBUG] [Thread Per-Client %ld] Exiting per_client_thread.\n", targ->thread_logical_id);
  free(targ);
  return NULL;
}


//------------------------------------------------------------------------------------------    
void *collector_handler_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg; // ID 1 for this thread
  fprintf(stderr, "[DEBUG] [Thread Collector Handler %ld] Entering collector_handler_thread\n", logical_id);
    
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
  fprintf(stderr, "[DEBUG] [Thread Collector Handler %ld] Socket created.\n", logical_id);


  if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    perror("[ERR] bind");
    close(server_fd);
    return (void *)-1; // Cast to void*
  }
  fprintf(stderr, "[DEBUG] [Thread Collector Handler %ld] Socket bound to port %d.\n", logical_id, port);

  if (listen(server_fd, MAX_CLIENTS) < 0) {
    perror("[ERR] listen");
    close(server_fd);
    return (void *)-1; // Cast to void*
  }
  printf("[INFO] [Thread Collector Handler %ld] Server started and listening on port %d\n", logical_id, port);


  while(1) {
    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(server_fd, &read_fds);
    int max_fd = server_fd;
    int new_sock; 
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    fprintf(stderr, "[DEBUG] [Thread Collector Handler %ld] Waiting for new connections...\n", logical_id);
    int activity = select(max_fd + 1, &read_fds, NULL, NULL, NULL);
    if (activity < 0 && errno != EINTR) {
      perror("[ERR] select");
      break;
    } 
    if (FD_ISSET(server_fd, &read_fds)) {
      new_sock = accept(server_fd, (struct sockaddr *)&client_addr, &client_len);
      if (new_sock < 0) {
        perror("[ERR] accept on event port");
        continue;
      } 
      fprintf(stderr, "[DEBUG] [Thread Collector Handler %ld] Accepted new connection on socket %d.\n", logical_id, new_sock);
      thread_arg_t *targ = malloc(sizeof(thread_arg_t));
      if (!targ) {
        perror("[ERR] malloc for thread_arg_t");
        close(new_sock);
        continue;
      }
      targ->sockfd = new_sock;
      targ->thread_logical_id = logical_id;  // ou générer un ID unique si tu veux

      pthread_t tid;
      if (pthread_create(&tid, NULL, per_client_thread, targ) != 0) {
        perror("[ERR] pthread_create per_client_thread");
        close(new_sock);
        free(targ);
        continue;
      }
      fprintf(stderr, "[DEBUG] [Thread Collector Handler %ld] Created new per-client thread with logical ID %ld.\n", logical_id, targ->thread_logical_id);
      pthread_detach(tid);
    }
  }
  fprintf(stderr, "[DEBUG] [Thread Collector Handler %ld] Exiting collector_handler_thread.\n", logical_id);
  close(server_fd);
  return NULL;
}
//------------------------------------------------------------------------------------------    

int main(int argc, char **argv) {
    signal(SIGINT, handle_sigint); // Set up signal handler 
    fprintf(stderr, "[DEBUG] [Main Thread] Entering main function.\n");
    char *plav[2] = {argv[0], "-q"};
    if (!PL_initialise(2, plav)) {
        fprintf(stderr, "[ERR] [Main Thread] Failure to initialize SWI-Prolog\n");
        return 1;
    }      
    fprintf(stderr, "[DEBUG] [Main Thread] SWI-Prolog initialized.\n");

    pthread_t collector_handler_tid;
    if (pthread_create(&collector_handler_tid, NULL, collector_handler_thread, (void *)THREAD_ID_COLLECTOR) != 0) {
        perror("[ERR] [Main Thread] pthread_create collector_handler_thread");
        PL_halt(1);
    }
    fprintf(stderr, "[DEBUG] [Main Thread] Collector handler thread created.\n");
    pthread_detach(collector_handler_tid);
    sleep(1);
    
   // Lancer le thread interne aux règles (ID 2)
    pthread_t severity_pthread_id;
    if (pthread_create(&severity_pthread_id, NULL, severity_updater_thread, (void *)THREAD_ID_SEVERITY) != 0) {
        perror("[ERR] [Main Thread] pthread_create severity_updater_thread");
        PL_halt(1);
    }
    fprintf(stderr, "[DEBUG] [Main Thread] Severity updater thread created.\n");
    pthread_detach(severity_pthread_id);


    // Do not close - infinite loop 
    // close(event_server_fd);
    pthread_join(collector_handler_tid, NULL); // Changed from collector_handler_thread to collector_handler_tid
    pthread_join(severity_pthread_id,  NULL); // Changed from severity_updater_thread to severity_pthread_id
    fprintf(stderr, "[DEBUG] [Main Thread] Exiting main function.\n");
    while(1) {sleep(10);}
    return 0;
}
