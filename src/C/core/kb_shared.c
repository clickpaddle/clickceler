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
void *kb_shared_handler_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg;
  fprintf(stderr,
          "[DEBUG] [Kb_shared Thread %ld] Entering kb_shared_handler_thread\n",
          logical_id);

  // Attach the C thread to the Prolog engine
  PL_engine_t kb_shared_engine = PL_thread_attach_engine(NULL);
  if (kb_shared_engine == NULL) {
    fprintf(stderr, "[Kb_shared %ld] Failed to attach Prolog engine\n",
            logical_id);
    return NULL;
  }

  // Load the "kb_shared" module
  const char *modname = "kb_shared";
  atom_t modname_atom = PL_new_atom_nchars(strlen(modname), modname);
  module_t mod = PL_new_module(modname_atom);

  if (load_rules_for_thread(kb_shared_engine, mod, modname_atom) != 0) {
    fprintf(stderr, "[Kb_shared %ld] Failed to load rules for module '%s'\n",
            logical_id, modname);
    PL_unregister_atom(modname_atom);
    PL_thread_destroy_engine();
    return NULL;
  }

  fprintf(stderr, "[Kb_shared %ld] Module '%s' successfully loaded.\n",
          logical_id, modname);

  // Optionally call kb_shared:start_kb_shared_loop/0
  const char *predicate = "start_kb_shared_loop";
  predicate_t pred_start = PL_predicate(predicate, 0, modname);

  if (pred_start) {
    qid_t q = PL_open_query(mod, PL_Q_NORMAL, pred_start, 0);
    if (!PL_next_solution(q)) {
      fprintf(
          stderr,
          "[ERROR] [Kb_shared %ld] Failed to run %s: no solution or error.\n",
          logical_id, predicate);
    } else {
      fprintf(stderr, "[DEBUG] [Kb_shared %ld] %s completed successfully.\n",
              logical_id, predicate);
    }
    PL_close_query(q);
  } else {
    fprintf(
        stderr,
        "[ERROR] [Kb_shared %ld] Predicate %s/0 not found in module '%s'.\n",
        logical_id, predicate, modname);
  }

  // Cleanup
  PL_unregister_atom(modname_atom);
  PL_thread_destroy_engine();

  fprintf(stderr,
          "[DEBUG] [Kb_shared %ld] Prolog engine detached and thread exited.\n",
          logical_id);
  return NULL;
}
