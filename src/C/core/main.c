#include <global.h>
#include <clickceler.h>

//---------------------------------------------------------------------------------------

int keep_running = 1;

void handle_sigint(int sig) {
  keep_running = 0;
  exit(0);
}

//------------------------------------------------------------------------------------------
static void redirect_all_output(const char *filename) {
    int fd = open(filename, O_WRONLY | O_CREAT | O_APPEND, 0644);
    if (fd < 0) {
        perror("open log file");
        exit(1);
    }

    // Redirige stdout et stderr vers le fichier
    if (dup2(fd, STDOUT_FILENO) < 0) perror("dup2 stdout");
    if (dup2(fd, STDERR_FILENO) < 0) perror("dup2 stderr");
    close(fd);

    // Pas de buffering pour stderr, buffering ligne pour stdout
    setvbuf(stdout, NULL, _IOLBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
}
//------------------------------------------------------------------------------------------
void create_and_detach_thread(pthread_t *tid, void *(*start_routine)(void *), int thread_id, const char *thread_name) {
  if (pthread_create(tid, NULL, start_routine, (void *)(intptr_t)thread_id) != 0) {
    fprintf(stderr, "[ERR] [Main Thread] pthread_create %s\n", thread_name);
    PL_halt(1);
  }
  fprintf(stderr, "[DEBUG] [Main Thread] %s thread created.\n", thread_name);
  pthread_detach(*tid);
}

//------------------------------------------------------------------------------------------
ThreadInfo threads[] = {
  {0, generic_rule_handler_thread, THREAD_ID_KB_SHARED, "Kb_shared handler"},
  {0, generic_rule_handler_thread, THREAD_ID_REFINE, "Refine handler"},
  {0, generic_rule_handler_thread, THREAD_ID_FILTER, "Filter handler"},
  {0, generic_rule_handler_thread, THREAD_ID_THROTTLE, "Throttle handler"},
  {0, generic_rule_handler_thread, THREAD_ID_ABSTRACT, "Abstract handler"},
  {0, generic_rule_handler_thread, THREAD_ID_UPDATE, "Update handler"},
  {0, generic_rule_handler_thread, THREAD_ID_CORRELATE, "Correlate handler"},
  {0, generic_rule_handler_thread, THREAD_ID_EXECUTE, "Execute handler"},
  {0, generic_rule_handler_thread, THREAD_ID_TIMER, "Timer handler"},
  {0, generic_rule_handler_thread, THREAD_ID_PROPAGATE, "Propagate handler"},
  {0, generic_rule_handler_thread, THREAD_ID_TRIGGER, "Trigger handler"}
};

//------------------------------------------------------------------------------------------
int main(int argc, char *argv[]) {
  int num_threads = sizeof(threads) / sizeof(ThreadInfo);
  signal(SIGINT, handle_sigint); // Set up signal handler
  fprintf(stderr, "[DEBUG] [Main Thread] Entering main function.\n");
  redirect_all_output("../logs/clickceler.log");
  char *plav[2] = {argv[0], "-q"};
  if (!PL_initialise(2, plav)) {
    fprintf(stderr, "[ERR] [Main Thread] Failure to initialize SWI-Prolog\n");
    return 1;
  }

  for (int i = 0; i < num_threads; i++) {
    create_and_detach_thread(&threads[i].tid,
                             threads[i].handler,
                             threads[i].thread_id,
                             threads[i].name);
    pthread_join(threads[i].tid, NULL);
  }

  // Collector Thread Created only when every thread is ready
  pthread_t collector_handler_tid;
  if (pthread_create(&collector_handler_tid, NULL, collector_rule_handler_thread,
                     (void *)THREAD_ID_COLLECTOR) != 0) {
    perror("[ERR] [Main Thread] pthread_create collector_handler_thread");
    PL_halt(1);
  }
  fprintf(stderr, "[DEBUG] [Main Thread] Collector handler thread created.\n");
  sleep(1);
  pthread_join(collector_handler_tid, NULL);

  fprintf(stderr, "[DEBUG] [Main Thread] Exiting main function.\n");

  while (1) {
    sleep(100);
  }

  return 0;
}

