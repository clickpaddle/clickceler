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
#include <string.h>


#define SERVER_PORT 65432
#define MAX_CLIENTS 10
#define MAX_LINE 8192

#define THREAD_ID_KB_SHARED 1
#define THREAD_ID_COLLECTOR 2
#define THREAD_ID_REFINE    3
#define THREAD_ID_FILTER    4 
#define THREAD_ID_THROTTLE  5 
#define THREAD_ID_ABSTRACT  6
#define THREAD_ID_UPDATE    7
#define THREAD_ID_CORRELATE  8
#define THREAD_ID_EXECUTE     9
#define THREAD_ID_TIMER       10
#define THREAD_ID_PROPAGATE   11
#define THREAD_ID_TRIGGER     12

typedef struct {
  int sockfd;
  intptr_t thread_logical_id; // ID logique (0 pour event, 1 pour query, 2 pour
                              // severity)
} thread_arg_t;


