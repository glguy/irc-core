#include "glirc-thread.h"

void free_thread_state(struct thread_state *st)
{
        lua_State *L = st->L;
        lua_pushnil(L);
        lua_rawsetp(L, LUA_REGISTRYINDEX, st);
}

struct thread_state *
new_thread_state(lua_State *L, size_t n)
{
        struct thread_state *st = lua_newuserdata(L, n);
        lua_rotate(L, -2, 1);
        lua_setuservalue(L, -2);
        lua_rawsetp(L, LUA_REGISTRYINDEX, st);
        st->L = L;
        return st;
}

void thread_state_value(struct thread_state *st)
{
        lua_State *L = st->L;
        lua_rawgetp(L, LUA_REGISTRYINDEX, st);
        lua_getuservalue(L, -1);
        lua_remove(L, -2);
}
