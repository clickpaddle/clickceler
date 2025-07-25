
#define SERVER_PORT 65432
#define MAX_CLIENTS 10
#define MAX_LINE 8192

#define THREAD_ID_COLLECTOR (void *)1
#define THREAD_ID_REFINE  (void *)2
#define THREAD_KB_BASE (void *)3
#define THREAD_KB_COMPLETE (void *)4
#define THREAD_KB_FILTER (void *)5
#define THREAD_KB_FLOWCTL (void *)6
#define THREAD_KB_UPDATE (void *)7
#define THREAD_KB_CONCEPT (void *)8
#define THREAD_KB_EXEC (void *)9
#define THREAD_KB_CLOCK (void *) 10
#define THREAD_KB_FORWARD (vois *)11


int load_rules_for_thread(PL_engine_t tid,module_t mod,atom_t modname);
void *collector_handler_thread(void *arg);
void *severity_updater_thread(void *arg);
void *refine_handler_thread(void *arg);
void *kb_shared_handler_thread(void *arg);
