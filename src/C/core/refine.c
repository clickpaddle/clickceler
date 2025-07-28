#define _DEFAULT_SOURCE
#include <SWI-Prolog.h>
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

void *refine_handler_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg;
  fprintf(stderr,
          "[DEBUG] [Refine Thread %ld] Entering refine_handler_thread\n",
          logical_id);

  // Attach the C thread to the Prolog engine

  PL_engine_t refine_engine = PL_thread_attach_engine(NULL);
  if (refine_engine == NULL) {
    fprintf(stderr, "[ERROR] [Refine %ld] Failed to attach Prolog engine\n",
            logical_id);
    return NULL;
  }

  // Create module "refine"
  const char *modname = "refine";
  atom_t modname_atom = PL_new_atom(modname);
  module_t mod = PL_new_module(modname_atom);

  // Load Rules
  if (load_rules_for_thread(refine_engine, mod, modname_atom)) {
    fprintf(stderr, "[ERROR] [Refine] Failed to load rules for module '%s'\n",
            modname);
    PL_thread_destroy_engine();
    return NULL;
  }

  fprintf(stderr, "[INFO] [Refine] Module '%s' successfully loaded.\n",
          modname);

  // call start_refine_loop/0 in the module
  const char *predicate = "start_refine_loop";
  predicate_t pred_start = PL_predicate(predicate, 0, modname);

  if (!pred_start) {
    fprintf(stderr,
            "[ERROR] [Refine %ld] Predicate %s/0 not found in module '%s'.\n",
            logical_id, predicate, modname);
    PL_unregister_atom(modname_atom);
    PL_thread_destroy_engine();
    return NULL;
  }

  qid_t q = PL_open_query(mod, PL_Q_NORMAL, pred_start, 0);
  if (!PL_next_solution(q)) {
    fprintf(
        stderr,
        "[ERROR] [Refine %ld] Failed to run %s/0 (no solution or exception).\n",
        logical_id, predicate);
  } else {
    fprintf(stderr, "[INFO] [Refine %ld] %s/0 returned (loop exited).\n",
            logical_id, predicate);
  }
  PL_close_query(q);

  PL_unregister_atom(modname_atom);
  PL_thread_destroy_engine();
  fprintf(stderr,
          "[DEBUG] [Refine %ld] Prolog engine detached and thread exited.\n",
          logical_id);
  return NULL;
}
