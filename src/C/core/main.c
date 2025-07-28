#include <global.h>
#include <clickceler.h>

//---------------------------------------------------------------------------------------

volatile int keep_running = 1;

void handle_sigint(int sig) {
  keep_running = 0;
  exit(0);
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

  // kb_shared_thread
  pthread_t kb_shared_handler_tid;
  if (pthread_create(&kb_shared_handler_tid, NULL, kb_shared_handler_thread,
                     (void *)THREAD_ID_KB_SHARED) != 0) {
    perror("[ERR] [Main Thread] pthread_create refine_handler_thread");
    PL_halt(1);
  }
  fprintf(stderr, "[DEBUG] [Main Thread] Refine handler thread created.\n");
  pthread_detach(kb_shared_handler_tid);
  sleep(1);

  // Collector Thread
  pthread_t collector_handler_tid;
  if (pthread_create(&collector_handler_tid, NULL, collector_handler_thread,
                     (void *)THREAD_ID_COLLECTOR) != 0) {
    perror("[ERR] [Main Thread] pthread_create collector_handler_thread");
    PL_halt(1);
  }
  fprintf(stderr, "[DEBUG] [Main Thread] Collector handler thread created.\n");
  pthread_detach(collector_handler_tid);
  sleep(1);

  // Refine Thread
  pthread_t refine_handler_tid;
  if (pthread_create(&refine_handler_tid, NULL, refine_handler_thread,
                     (void *)THREAD_ID_REFINE) != 0) {
    perror("[ERR] [Main Thread] pthread_create refine_handler_thread");
    PL_halt(1);
  }
  fprintf(stderr, "[DEBUG] [Main Thread] Refine handler thread created.\n");
  pthread_detach(refine_handler_tid);
  sleep(1);

  // Do not close - infinite loop
  // close(event_server_fd);
  pthread_join(collector_handler_tid, NULL);
  pthread_join(refine_handler_tid, NULL);
  fprintf(stderr, "[DEBUG] [Main Thread] Exiting main function.\n");
  while (1) {
    sleep(10);
  }
  return 0;
}
