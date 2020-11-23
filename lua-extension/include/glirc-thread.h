#ifndef GLIRC_LUA_THREAD
#define GLIRC_LUA_THREAD 1

#include <lua.h>

struct thread_state;

typedef void on_join(struct thread_state *);

struct thread_state {
    lua_State *L;
    on_join *on_join_fun;
};

void thread_join_entrypoint(void *R);
struct thread_state * new_thread_state(struct lua_State *L, size_t n, on_join *f);

#endif
