#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "glirc-api.h"

#define PROC_KEY "glirc-process-message-callback"

static void *start(void) {
        lua_State *L = luaL_newstate();
        luaL_openlibs(L);
        int res = luaL_dofile(L, "glirc.lua");
        if (res == LUA_OK) {
          lua_setfield(L, LUA_REGISTRYINDEX, PROC_KEY);
        }
        return L;
}

static void stop(void * S) {
        lua_State *L = S;
        lua_close(L);
}

static void push_glirc_string(lua_State *L, const struct glirc_string *s) {
        lua_pushlstring(L, s->str, s->len);
}

static void push_glirc_message
  ( lua_State *L
  , const struct glirc_message *msg
  ) {
        lua_createtable(L, 0, 4);

        push_glirc_string(L, &msg->network);
        lua_setfield(L,-2,"network");

        push_glirc_string(L, &msg->prefix);
        lua_setfield(L,-2,"prefix");

        push_glirc_string(L, &msg->command);
        lua_setfield(L,-2,"command");

        lua_createtable(L, msg->params_n, 0);
        for (size_t i = 0; i < msg->params_n; i++) {
                push_glirc_string(L, &msg->params[i]);
                lua_seti(L, -2, i+1);
        }
        lua_setfield(L,-2,"params");
}

static void process_message(void * S, const struct glirc_message *msg) {
        lua_State *L = S;
        int ty = lua_getfield(L, LUA_REGISTRYINDEX, PROC_KEY);
        push_glirc_message(L, msg);
        int res = lua_pcall(L, 1, 0, 0);
}

struct glirc_extension extension = {
        .start = start,
        .stop  = stop,
        .process_message = process_message
};
