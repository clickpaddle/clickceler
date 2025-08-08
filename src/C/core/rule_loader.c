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

//-------------------------------------------------------------------------
// Load rules for Prolog Thread

int load_rules_for_thread(PL_engine_t prolog_engine, module_t out_module,
                          atom_t modname_atom) {
  unsigned long current_thread_id = (unsigned long)pthread_self();

  // Convert atom_t to string
  const char *modname_str = NULL;
  term_t modname_term = PL_new_term_ref();
  PL_put_atom(modname_term, modname_atom);
  if (!PL_get_atom_chars(modname_term, &modname_str)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Cannot convert atom to string\n",
            current_thread_id);
    return 1;
  }

  // Build file path: ../engine/<modulename>.pl
  char filename[256];
  snprintf(filename, sizeof(filename), "../engine/%s.pl", modname_str);

  // Load file into module
  char load_goal_str[512];
  snprintf(load_goal_str, sizeof(load_goal_str),
           "load_files('%s', [if(true), module(%s)])", filename, modname_str);

  term_t load_term = PL_new_term_ref();
  if (!PL_chars_to_term(load_goal_str, load_term)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Invalid load_files goal: %s\n",
            current_thread_id, load_goal_str);
    return 1;
  }

  if (!PL_call(load_term, NULL)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Failed to load file: %s\n",
            current_thread_id, filename);
    return 1;
  }

  fprintf(stderr, "[DEBUG] [Thread %lu] Rules loaded from %s\n",
          current_thread_id, filename);

  // Prepare thread_goal_<modnam>: with_output_to(string(S), thread_goal_<modname>(ThreadID))
  char goal_name[128];
  snprintf(goal_name, sizeof(goal_name), "thread_goal_%s", modname_str);

  predicate_t pred = PL_predicate(goal_name, 1, modname_str);
  if (!pred) {
    fprintf(stderr,
            "[ERROR] [Thread %lu] Predicate %s/1 not found in module %s\n",
            current_thread_id, goal_name, modname_str);
    return 1;
  }

  // Build Prolog call: with_output_to(string(S),
  // thread_goal_<modname>(ThreadID))
  term_t goal = PL_new_term_refs(3);
  term_t string_var = goal;     // arg 1 of with_output_to
  term_t inner_call = goal + 1; // thread_goal_<modname>(ThreadID)
  term_t full_call = goal + 2;  // with_output_to(...)

  // Build thread ID term
  term_t thread_id_term = PL_new_term_ref();
  PL_put_integer(thread_id_term, (intptr_t)current_thread_id);

  // Build inner goal: thread_goal_<modname>(ThreadID)
  if (!PL_cons_functor(inner_call, PL_new_functor(PL_new_atom(goal_name), 1),
                       thread_id_term)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Failed to construct goal %s/1\n",
            current_thread_id, goal_name);
    return 1;
  }

  // Build full call: with_output_to(string(S), inner_call)
  functor_t f_with_output = PL_new_functor(PL_new_atom("with_output_to"), 2);
  functor_t f_string = PL_new_functor(PL_new_atom("string"), 1);

  term_t string_functor = PL_new_term_ref();
  if (!PL_cons_functor(string_functor, f_string, string_var)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Failed to build string(S)\n",
            current_thread_id);
    return 1;
  }

  if (!PL_cons_functor(full_call, f_with_output, string_functor, inner_call)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Failed to build with_output_to/2\n",
            current_thread_id);
    return 1;
  }

  // Call Prolog goal
  if (!PL_call(full_call, out_module)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Call to %s/1 failed\n",
            current_thread_id, goal_name);
    return 1;
  }

  // Get captured output
  char *captured_output;
  if (!PL_get_chars(string_var, &captured_output,
                    CVT_ALL | CVT_WRITE | BUF_DISCARDABLE)) {
    fprintf(stderr, "[ERROR] [Thread %lu] Failed to extract output string\n",
            current_thread_id);
    return 1;
  }

  fprintf(stderr, "[INFO] [Thread %lu] Captured output: %s\n",
          current_thread_id, captured_output);
  return 0;
}
