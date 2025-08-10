
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
  pthread_t tid;
  void *(*handler)(void *);
  int thread_id;
  const char *name;
} ThreadInfo;

const char *get_module_name_from_id(int id);
int load_rules_for_thread(PL_engine_t tid,module_t mod,atom_t modname);
void *generic_rule_handler_thread(void *arg);
void *collector_rule_handler_thread(void *arg);
