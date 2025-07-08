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
// Load rules for Prolog Thread
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
           "load_files('%s', [if(true), module(%s)])", filename, modname);
  
  term_t goal = PL_new_term_ref();
  if (!PL_chars_to_term(goal_buf, goal) || !PL_call(goal, NULL)) {
    fprintf(stderr, "[ERR] Thread %ld: échec chargement %s\n", (long)tid,
            filename); 
    return 0;
  } 
    
  // Init Module
  predicate_t p = PL_predicate(&thread_goal, 1, modname);
  term_t t = PL_new_term_ref();
  PL_put_integer(t, (long)tid);
  (void)PL_call_predicate(*out_module, PL_Q_NORMAL, p, t);

  return 1;
}   
  
//------------------------------------------------------------------------------------------    

// Thread  to run internal rules (client_id 2)
void *severity_updater_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg; // ID 2 for this thread

  if (!PL_thread_attach_engine(NULL)) {
    fprintf(
        stderr,
        "[ERR] Severitviy Updater Thread %ld: PL_thread_attach_engine failed\n",
        logical_id);
    return NULL;
  }

  module_t mod;
  if (!load_rules_for_thread(logical_id, &mod)) { // Load rules_2.pl
    if (!mod) { fprintf(stderr,"[ERROR] Module context is NULL -- Prolog ruleset not loaded properly.\n");}
    PL_thread_destroy_engine();
    return NULL;
  }

  printf("[Thread internal rules %ld] started. Call update_event_severity...\n",
         logical_id);

  // Check that update_event_severity/0 is defined in dans rules_2.pl
  predicate_t p_update_severity = PL_predicate("update_event_severity", 0, mod);
  if (!p_update_severity) {
    fprintf(stderr,
            "[ERR] Thread Internal rules %ld: Predicate "
            "'update_event_severity/0' s not in ruleset '%s'.\n",
            logical_id, PL_module_name(mod));
    PL_thread_destroy_engine();
    return NULL;
   }
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
        sleep(10); // update every 10 second (ajustez si besoin)
    }

}

//------------------------------------------------------------------------------------------    
char *execute_prolog_query_with_output(const char *query_str, atom_t mod) {
  // Step 1: Parse the predicate name from query_str
    char predicate_name[256];
    if (sscanf(query_str, "%255[^ (]", predicate_name) != 1) {
        fprintf(stderr, "[ERROR] Could not parse predicate from: %s\n", query_str);
        return NULL;
    }
  
  printf("Predicate_name %s\n",predicate_name);
  // Step 2: Construct goal: predicate_name(Var)
  functor_t functor = PL_new_functor(PL_new_atom(predicate_name), 1);
  term_t var = PL_new_term_ref();      // This will capture the result
  term_t goal = PL_new_term_ref();     // This holds the full call: functor(var)
  PL_cons_functor(goal, functor, var); // goal = predicate_name(var)
  
  // Step 3: Call Prolog
  qid_t qid = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, goal, NULL);
  
  char *output = NULL;
  
  if (PL_next_solution(qid)) {
    // Extract result
    char *str;
    if (PL_get_chars(var, &str, CVT_WRITE | CVT_ALL | BUF_DISCARDABLE)) {
      output = strdup(str); // Copy result so it's safe to return
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
  return output; // Might be NULL on error
}
//------------------------------------------------------------------------------------------    
void *collector_handler(long int logical_id,int sockfd,module_t mod){
  ssize_t nbytes;
  char buffer[MAX_LINE];
  printf("[Thread Collect %ld] ready to receive JSON EVENT or QUERY.\n", logical_id);
  while ((nbytes = recv(sockfd, buffer, sizeof(buffer) - 1, 0)) > 0) {
    buffer[nbytes] = '\0';
    if (strncmp(buffer, "EVENT ", 6) == 0) {
      char *event_json = buffer + 6;
      predicate_t p_assert = PL_predicate("assert_json_event", 0, mod);
      if (!p_assert) {
        fprintf(stderr,
                "[ERR] Collector Thread %ld: Can't find predicate "
                "'assert_json_event/2' in ruleset '%s'.\n",
                logical_id, PL_module_name(mod));
        PL_thread_destroy_engine();
        close(sockfd);
        return NULL;
      }
      printf("[Thread Collecte %ld] Ready to receive EVENT: %s\n", logical_id, event_json);
      term_t args = PL_new_term_refs(2);
      PL_put_integer(args, (long)logical_id);
      PL_put_atom_chars(args + 1, event_json);

      qid_t q = PL_open_query(mod, PL_Q_CATCH_EXCEPTION, p_assert, args);
      if (!PL_next_solution(q)) {
        term_t ex = PL_exception(q);
        if (ex) {
          char *ex_str = NULL;
          if (PL_get_chars(ex, &ex_str, CVT_ALL)) {
            fprintf(stderr, "[Thread Collecte %ld] Prolog Error: %s\n",
                    logical_id, ex_str);
          } else {
            fprintf(stderr,
                    "[Thread Collecte %ld] Prolog Error (non-stringifiable "
                    "exception)\n",
                    logical_id);
          }
        }
      }
      PL_close_query(q);

   } else if (strncmp(buffer, "QUERY ", 6) == 0) {
      char *query_str = buffer + 6;

      printf("[Thread Collecte %ld] Received QUERY: %s\n", logical_id,
             query_str);
      char *result = execute_prolog_query_with_output(query_str, PL_new_atom(PL_module_name(mod)));
      if (result) {
        send(sockfd, result, strlen(result), 0);
        free(result);
      } else {
        send(sockfd, "Error: Could not retrieve results.\n", 35, 0);
      }
    } else {
      fprintf(stderr,
              "[Thread Collecte %ld] Reçu message inconnu (ni EVENT ni QUERY): "
              "%s\n",
              logical_id, buffer);
    }
  }
}

//------------------------------------------------------------------------------------------    
void *per_client_thread(void *arg) {
  thread_arg_t *targ = (thread_arg_t *)arg;

  if (!PL_thread_attach_engine(NULL)) {
    fprintf(stderr, "[ERR] Collector Thread %ld: PL_thread_attach_engine failed\n", targ->thread_logical_id);
    close(targ->sockfd);
    free(targ);
    return NULL;
  }

  module_t mod;
  if (!load_rules_for_thread(targ->thread_logical_id, &mod)) {
    fprintf(stderr, "[ERR] Failed to load rules for thread %ld\n", targ->thread_logical_id);
    PL_thread_destroy_engine();
    close(targ->sockfd);
    free(targ);
    return NULL;
  }

  collector_handler(targ->thread_logical_id, targ->sockfd, mod);

  close(targ->sockfd);
  PL_thread_destroy_engine();
  free(targ);
  return NULL;
}


//------------------------------------------------------------------------------------------    
void *collector_handler_thread(void *arg) {

    
  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY; 
  uint16_t port = SERVER_PORT;
  addr.sin_port = htons(port);
  int server_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (server_fd < 0) {
    perror("socket creation failed");
    PL_halt(1);
  }

  if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    perror("[ERR] bind");
    close(server_fd);
    return -1;
  }

  if (listen(server_fd, MAX_CLIENTS) < 0) {
    perror("[ERR] listen");
    close(server_fd);
    return -1;
  }
  printf("[INFO] Server started and listening on port %d\n", port);


  while(1) {
    intptr_t logical_id = (intptr_t)arg; // ID 3 for this thread
    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(server_fd, &read_fds);
    int max_fd = server_fd;
    int new_sock; 
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

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
      thread_arg_t *targ = malloc(sizeof(thread_arg_t));
      targ->sockfd = new_sock;
      targ->thread_logical_id = logical_id;  // ou générer un ID unique si tu veux

      pthread_t tid;
      if (pthread_create(&tid, NULL, per_client_thread, targ) != 0) {
        perror("[ERR] pthread_create per_client_thread");
        close(new_sock);
        free(targ);
        continue;
      }
      pthread_detach(tid);
    }
  }
}
//------------------------------------------------------------------------------------------    

int main(int argc, char **argv) {
    char *plav[2] = {argv[0], "-q"};
    if (!PL_initialise(2, plav)) {
        fprintf(stderr, "[ERR] Failure to initialize SWI-Prolog\n");
        return 1;
    }      

    pthread_t collector_handler_tid;
    if (pthread_create(&collector_handler_tid, NULL, collector_handler_thread, (void *)THREAD_ID_COLLECTOR) != 0) {
        perror("[ERR] pthread_create collector_handler_thread");
        PL_halt(1);
    }
    pthread_detach(collector_handler_tid);
    sleep(1);
   // Lancer le thread interne aux règles (ID 2)
    pthread_t severity_pthread_id;
    if (pthread_create(&severity_pthread_id, NULL, severity_updater_thread, (void *)THREAD_ID_SEVERITY) != 0) {
        perror("[ERR] pthread_create severity_updater_thread");
        PL_halt(1);
    }
    pthread_detach(severity_pthread_id);


    // Do not close - infinite loop 
    // close(event_server_fd);
    pthread_join(collector_handler_thread, NULL);
    pthread_join(severity_updater_thread,  NULL);
    return 0;
}

