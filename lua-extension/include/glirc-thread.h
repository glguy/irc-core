#ifndef GLIRC_LUA_THREAD
#define GLIRC_LUA_THREAD 1

#include <lua.h>

struct thread_state {
    lua_State *L;
};

void free_thread_state(struct thread_state *st);
struct thread_state * new_thread_state(struct lua_State *L, size_t n);
void thread_state_value(struct thread_state *st);

#endif
