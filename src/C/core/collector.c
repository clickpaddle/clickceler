#define _DEFAULT_SOURCE
#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <errno.h>
#include <global.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

extern int keep_running;

//------------------------------------------------------------------------

// Parse the string "module:predicate(args)" or "predicate(args)" and extract
// module, pred, args (optional)

static void parse_query_string(const char *query_str, char *module,
                               size_t mod_len, char *pred, size_t pred_len,
                               char *args, size_t args_len) {
  module[0] = '\0';
  pred[0] = '\0';
  args[0] = '\0';

  const char *p_colon = strchr(query_str, ':');
  if (p_colon) {
    // There is a module
    size_t mod_size = p_colon - query_str;
    if (mod_size >= mod_len)
      mod_size = mod_len - 1;
    strncpy(module, query_str, mod_size);
    module[mod_size] = '\0';

    const char *p_pred = p_colon + 1;
    // search for '(' in the rest
    const char *p_paren = strchr(p_pred, '(');
    if (p_paren) {
      size_t pred_size = p_paren - p_pred;
      if (pred_size >= pred_len)
        pred_size = pred_len - 1;
      strncpy(pred, p_pred, pred_size);
      pred[pred_size] = '\0';

      // Arguments between '(' and ')'
      const char *p_end_paren = strchr(p_paren, ')');
      if (p_end_paren) {
        size_t args_size = p_end_paren - (p_paren + 1);
        if (args_size >= args_len)
          args_size = args_len - 1;
        strncpy(args, p_paren + 1, args_size);
        args[args_size] = '\0';
      }
    } else {
      // No arguments
      strncpy(pred, p_pred, pred_len - 1);
      pred[pred_len - 1] = '\0';
    }
  } else {
    // No module, default to user
    strncpy(module, "user", mod_len);
    module[mod_len - 1] = '\0';

    // Search '('
    const char *p_paren = strchr(query_str, '(');
    if (p_paren) {
      size_t pred_size = p_paren - query_str;
      if (pred_size >= pred_len)
        pred_size = pred_len - 1;
      strncpy(pred, query_str, pred_size);
      pred[pred_size] = '\0';

      // Arguments
      const char *p_end_paren = strchr(p_paren, ')');
      if (p_end_paren) {
        size_t args_size = p_end_paren - (p_paren + 1);
        if (args_size >= args_len)
          args_size = args_len - 1;
        strncpy(args, p_paren + 1, args_size);
        args[args_size] = '\0';
      }
    } else {
      // No arguments
      strncpy(pred, query_str, pred_len - 1);
      pred[pred_len - 1] = '\0';
    }
  }
}
//------------------------------------------------------------------------------------
// Count the number of arguments in the args string separated by ','

static int count_args(const char *args) {
  if (args == NULL || *args == '\0')
    return 0;

  int count = 1;
  const char *p = args;
  while (*p) {
    if (*p == ',')
      count++;
    p++;
  }
  return count;
}

//------------------------------------------------------------------------------------
char *execute_prolog_query_with_output(const char *query_str,
                                       atom_t module_atom) {
  fprintf(stderr,
          "[DEBUG] Entering execute_prolog_query_with_output for query: %s\n",
          query_str);

  char module[128];
  char predicate[128];
  char args_str[512];
  const char *mod_str = PL_atom_chars(module_atom);
  strncpy(module, mod_str, sizeof(module) - 1);
  module[sizeof(module) - 1] = '\0';
  parse_query_string(query_str, module, sizeof(module), predicate,
                     sizeof(predicate), args_str, sizeof(args_str));

  int arity = count_args(args_str);
  fprintf(stderr,
          "[DEBUG] Parsed module='%s', predicate='%s', arity=%d, args='%s'\n",
          module, predicate, arity, args_str);

  predicate_t pred = PL_predicate(predicate, arity, module);
  if (!pred) {
    fprintf(stderr, "[ERROR] Predicate %s/%d in module %s not found.\n",
            predicate, arity, module);
    return NULL;
  }

  term_t term_refs = PL_new_term_refs(arity);

  // If arity >= 1, parse the first arity - 1 arguments as atoms,
  // and keep the last term as an output variable.

  if (arity > 0) {
    char *args_dup = strdup(args_str);
    char *tok = strtok(args_dup, ",");
    int i = 0;
    while (tok && i < arity - 1) {
      while (*tok == ' ')
        tok++;
      size_t len = strlen(tok);
      while (len > 0 && tok[len - 1] == ' ') {
        tok[len - 1] = '\0';
        len--;
      }

      PL_put_atom_chars(term_refs + i, tok);
      tok = strtok(NULL, ",");
      i++;
    }
    free(args_dup);

    // Last argument : variable to get the result string
    PL_put_variable(term_refs + (arity - 1));
  }

  qid_t qid = PL_open_query(module_atom, PL_Q_NORMAL, pred, term_refs);
  char *result = NULL;

  if (PL_next_solution(qid)) {
    char *tmp = NULL;
    if (PL_get_chars(term_refs + (arity - 1), &tmp,
                     CVT_ALL | BUF_DISCARDABLE)) {
      result = strdup(tmp); // we duplicate it so that the buffer belongs to us

    } else {
      fprintf(stderr,
              "[ERROR] PL_get_chars failed to extract output string.\n");
    }
  } else {
    fprintf(stderr, "[DEBUG] No solution from query.\n");
  }

  PL_close_query(qid);
  fprintf(stderr, "[Collector] [result] %s \n", result);
  return result;
}
//------------------------------------------------------------------------------------------

static void *collector_handler(PL_engine_t logical_id, int sockfd,
                               atom_t mod_atom, module_t mod) {
  fprintf(stderr,
          "[DEBUG] [Collector %ld] Entering collector_handler for socket %d\n",
          (long)logical_id, sockfd);

  ssize_t nbytes;
  char buffer[MAX_LINE];

  printf("[Collector] ready to receive JSON EVENT or QUERY.\n");

  while ((nbytes = recv(sockfd, buffer, sizeof(buffer) - 1, 0)) > 0) {
    buffer[nbytes] = '\0';
    fprintf(stderr, "[Collector] Received %zd bytes: %s\n", nbytes, buffer);

    if (strncmp(buffer, "EVENT ", 6) == 0) {
      char *event_json = buffer + 6;
      fprintf(stderr, "[DEBUG] [Collector %ld] Identified as EVENT. JSON: %s\n",
              (long)logical_id, event_json);

      term_t t = PL_new_term_ref();
      PL_put_atom(t, mod_atom); // ← convert atom_t into a term_t

      char *mod_name = NULL;
      if (!PL_get_atom_chars(t, &mod_name)) {
        fprintf(stderr, "Could not extract mod name from atom\n");
      } else {
        fprintf(stderr, "Module name: %s\n", mod_name);
      }
      // Get predicate handle for collect_events/1 in the specified module
      predicate_t pred = PL_predicate("collect_events", 1, mod_name);
      if (!pred) {
        fprintf(stderr,
                "[ERR] [Collector %ld] Can't find predicate 'collect_events/1' "
                "in module '%s'.\n",
                (long)logical_id, mod_name);
        continue;
      }

      // Create a Prolog term reference for the parsed JSON.
      // This term will be the single argument for collect_events/1.
      // Create Prolog term for JSON string (as atom)
      term_t arg = PL_new_term_ref();
      if (!PL_put_atom_chars(arg, event_json)) {
        fprintf(stderr,
                "[ERR] [Collector %ld] Failed to create Prolog atom from JSON "
                "string\n",
                (long)logical_id);
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
            fprintf(stderr, "[ERR] [Collector %ld] Prolog exception: %s\n",
                    (long)logical_id, err_msg);
          } else {
            fprintf(stderr,
                    "[ERR] [Collector %ld] Prolog exception (could not extract "
                    "message)\n",
                    (long)logical_id);
          }
        } else {
          fprintf(stderr,
                  "[ERR] [Collector %ld] collect_events/1 failed with no "
                  "exception.\n",
                  (long)logical_id);
        }
      } else {
        fprintf(stderr, "[DEBUG] [Collector %ld] collect_events/1 succeeded.\n",
                (long)logical_id);
      }

      PL_close_query(q);

    } else if (strncmp(buffer, "QUERY ", 6) == 0) {
      char *query_str = buffer + 6;

      printf("[Collector %ld] Received QUERY: %s\n", logical_id, query_str);
      fprintf(stderr,
              "[DEBUG] [Collector %ld] Identified as QUERY. Query string: %s\n",
              (long)logical_id, query_str);
      atom_t mod_atom = PL_new_atom("collector");
      char *result = execute_prolog_query_with_output(query_str, mod_atom);
      if (result) {
        size_t len = strlen(result);
        size_t total_sent = 0;
        ssize_t sent;

        while (total_sent < len) {
          sent = send(sockfd, result + total_sent, len - total_sent, 0);
          if (sent <= 0) {
            perror("[ERROR] send failed");
            break;
          }
          total_sent += sent;
        }

        sent = send(sockfd, "\n", 1, 0);
        if (sent <= 0) {
          perror("[ERROR] send failed");
        }

        fprintf(stderr, "[DEBUG] [Collector %ld] Sent query result: %s\n",
                logical_id, result);
        free(result);
      } else {
        const char *error_msg = "Error: Could not retrieve results.\n";
        size_t err_len = strlen(error_msg);
        size_t total_sent = 0;
        ssize_t sent;

        while (total_sent < err_len) {
          sent = send(sockfd, error_msg + total_sent, err_len - total_sent, 0);
          if (sent <= 0) {
            perror("[ERROR] send failed");
            break;
          }
          total_sent += sent;
        }

        fprintf(stderr, "[ERROR] [Collector %ld] Failed to get query result.\n",
                logical_id);
      }

      // Close socket in anycase
      if (close(sockfd) == -1) {
        perror("[ERROR] close failed");
      }

    } else {
      fprintf(stderr,
              "[Collector] Neither EVENT, nor QUERY received.\n "
              "%s\n",
              buffer);
    }
  }
  fprintf(stderr, "[DEBUG] [Collector] Client disconnected from socket %d.\n",
          sockfd);
  return NULL; // Ensure a return value for void*
}

//------------------------------------------------------------------------------------------

static void *per_client_thread(void *arg) {
  thread_arg_t *targ = (thread_arg_t *)arg;
  fprintf(stderr,
          "[DEBUG] [Collector %ld] Starting per_client_thread for socket %d\n",
          targ->thread_logical_id, targ->sockfd);
  PL_engine_t collector_engine = PL_thread_attach_engine(NULL);
  if (collector_engine == NULL) {
    fprintf(stderr, "[ERR] [Collector %ld]: PL_thread_attach_engine failed\n",
            targ->thread_logical_id);
    close(targ->sockfd);
    free(targ);
    return NULL;
  }
  fprintf(stderr, "[DEBUG] [Collector %ld] Prolog engine attached.\n",
          targ->thread_logical_id);
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
  collector_handler(collector_engine, targ->sockfd, modname_atom, mod);
  PL_unregister_atom(modname_atom);
  fprintf(stderr, "[Collector] Collector handler finished.\n");

  close(targ->sockfd);
  PL_thread_destroy_engine();
  fprintf(stderr, "[DEBUG] [Collector %ld] Exiting per_client_thread.\n",
          targ->thread_logical_id);
  free(targ);
  return NULL;
}

//------------------------------------------------------------------------------------------
void *collector_rule_handler_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg;
  fprintf(stderr,
          "[DEBUG] [Collector Handler %ld] Entering collector_handler_thread\n",
          logical_id);

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
  fprintf(stderr, "[DEBUG] [Collector Handler %ld] Socket created.\n",
          logical_id);

  if (bind(server_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    perror("[ERR] bind");
    close(server_fd);
    return (void *)-1; // Cast to void*
  }
  fprintf(stderr, "[DEBUG] [Collector Handler %ld] Socket bound to port %d.\n",
          logical_id, port);

  if (listen(server_fd, MAX_CLIENTS) < 0) {
    perror("[ERR] listen");
    close(server_fd);
    return (void *)-1; // Cast to void*
  }
  printf("[INFO] [Collector Handler %ld] Server started and listening on port "
         "%d\n",
         logical_id, port);

  while (1) {
    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(server_fd, &read_fds);
    int max_fd = server_fd;
    int new_sock;
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    fprintf(stderr,
            "[DEBUG] [Collector Handler %ld] Waiting for new connections...\n",
            logical_id);
    int activity = select(max_fd + 1, &read_fds, NULL, NULL, NULL);
    if (activity < 0 && errno != EINTR) {
      perror("[ERR] select");
      break;
    }
    if (FD_ISSET(server_fd, &read_fds)) {
      new_sock =
          accept(server_fd, (struct sockaddr *)&client_addr, &client_len);
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

      targ->thread_logical_id =
          (uintptr_t)pthread_self(); // Stores the casted numerical ID

      fprintf(stderr,
              "[DEBUG] [Collector Handler %zu] Accepted new connection on "
              "socket %d.\n",
              targ->thread_logical_id,
              new_sock); // Uses the numerical ID of the client thread

      pthread_t tid;
      if (pthread_create(&tid, NULL, per_client_thread, targ) != 0) {
        fprintf(stderr,
                "[DEBUG] [Collector Handler %zu] failed to create thread.\n",
                targ->thread_logical_id);
        free(targ); // Free memory in case of thread creation failure
        close(new_sock);
        continue;
      }
      // Detach the thread if you don't plan to join it
      // or store `tid` for a later `pthread_join`.
      fprintf(stderr, "[DEBUG] [Collector Handler %zu] created thread.\n",
              targ->thread_logical_id);
      pthread_detach(tid);
    }
  }
  fprintf(stderr,
          "[DEBUG] [Collector Handler %ld] Exiting collector_handler_thread.\n",
          logical_id);
  close(server_fd);
  return NULL;
}
//------------------------------------------------------------------------------------------
