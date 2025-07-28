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
#include <string.h>


#define SERVER_PORT 65432
#define MAX_CLIENTS 10
#define MAX_LINE 8192

#define THREAD_ID_KB_SHARED (void *)1
#define THREAD_ID_COLLECTOR (void *)2
#define THREAD_ID_SEVERITY  (void *)3
#define THREAD_KB_BASE (void *)4
#define THREAD_KB_COMPLETE (void *)5
#define THREAD_KB_FILTER (void *)6
#define THREAD_KB_FLOWCTL (void *)7
#define THREAD_KB_UPDATE (void *)8
#define THREAD_KB_CONCEPT (void *)9
#define THREAD_KB_EXEC (void *)10
#define THREAD_KB_CLOCK (void *)11
#define THREAD_KB_FORWARD (vois *)12


typedef struct {
  int sockfd;
  intptr_t thread_logical_id; // ID logique (0 pour event, 1 pour query, 2 pour
                              // severity)
} thread_arg_t;


