#include "glirc-thread.h"

void
thread_join_entrypoint(void *R) {
        struct thread_state *st = R;
        lua_State *L = st->L;

        if (st->on_join_fun != NULL) {
                st->on_join_fun(st);
        }

        lua_pushnil(L);
        lua_rawsetp(L, LUA_REGISTRYINDEX, st);
}

struct thread_state *
new_thread_state(struct lua_State *L, size_t n, on_join *f)
{
    struct thread_state *st = lua_newuserdata(L, n);
    lua_rotate(L, -2, 1);
    lua_setuservalue(L, -2);
    lua_rawsetp(L, LUA_REGISTRYINDEX, st);
    st->L = L;
    st->on_join_fun = f;
    return st;
}
