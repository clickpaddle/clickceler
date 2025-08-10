#define _DEFAULT_SOURCE
#include <SWI-Prolog.h>
#include <clickceler.h>
#include <errno.h>
#include <global.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

extern int keep_running;

const char *get_module_name_from_id(int id) {
  switch (id) {
  case THREAD_ID_KB_SHARED:
    return "kb_shared";
  case THREAD_ID_REFINE:
    return "refine";
  case THREAD_ID_FILTER:
    return "filter";
  case THREAD_ID_THROTTLE:
    return "throttle";
  case THREAD_ID_ABSTRACT:
    return "abstract";
  case THREAD_ID_UPDATE:
    return "update";
  case THREAD_ID_CORRELATE:
    return "correlate";
  case THREAD_ID_EXECUTE:
    return "execute";
  case THREAD_ID_TIMER:
    return "timer";
  case THREAD_ID_PROPAGATE:
    return "propagate";
  case THREAD_ID_TRIGGER:
    return "trigger";
  default:
    return NULL;
  }
}

void *generic_rule_handler_thread(void *arg) {
  intptr_t logical_id = (intptr_t)arg;

  const char *modname = get_module_name_from_id(logical_id);
  if (!modname) {
    fprintf(stderr, "[unknown thread engine] [ERROR] Unknown module ID %ld.\n",
            logical_id);
    return NULL;
  }

  fprintf(stderr, "[%s thread engine] [DEBUG] Thread %ld started.\n", modname,
          logical_id);

  PL_engine_t engine = PL_thread_attach_engine(NULL);
  if (engine == NULL) {
    fprintf(
        stderr,
        "[%s thread engine] [ERROR] Failed to attach Prolog engine (ID: %ld)\n",
        modname, logical_id);
    return NULL;
  }

  atom_t modname_atom = PL_new_atom(modname);
  module_t mod = PL_new_module(modname_atom);

  if (load_rules_for_thread(engine, mod, modname_atom)) {
    fprintf(stderr,
            "[%s thread engine] [ERROR] Failed to load rules for module '%s'\n",
            modname, modname);
    PL_thread_destroy_engine();
    return NULL;
  }

  fprintf(stderr,
          "[%s thread engine] [INFO] Module '%s' successfully loaded.\n",
          modname, modname);
  char predicate_name[64];
  snprintf(predicate_name, sizeof(predicate_name), "start_%s_loop", modname);

  // Look for Predicate start_<modname>_loop/0 n the module name modname
  predicate_t pred = PL_predicate(predicate_name, 0, modname);

  if (!pred) {
    fprintf(stderr,
            "[%s thread engine] [ERROR] Predicate %s/0 not found in module "
            "'%s' (ID: %ld).\n",
            modname, predicate_name, modname, logical_id);
    PL_unregister_atom(modname_atom);
    PL_thread_destroy_engine();
    return NULL;
  }

  //  Call Predicate  start_<modname>_loop/0
  qid_t q = PL_open_query(mod, PL_Q_NORMAL, pred, 0);
  if (!PL_next_solution(q)) {
    fprintf(stderr,
            "[%s thread engine] [ERROR] %s/0 failed or threw an exception (ID: "
            "%ld).\n",
            modname, predicate_name, logical_id);
  } else {
    fprintf(stderr,
            "[%s thread engine] [INFO] %s/0 returned successfully (ID: %ld).\n",
            modname, predicate_name, logical_id);
  }
  PL_close_query(q);

  // Cleanup
  PL_unregister_atom(modname_atom);
  PL_thread_destroy_engine();

  fprintf(stderr,
          "[%s thread engine] [DEBUG] Prolog engine detached (ID: %ld).\n",
          modname, logical_id);

  return NULL;
}
