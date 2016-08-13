#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "glirc-api.h"

#define PROC_KEY "glirc-process-message-callback"

/* Start the Lua interpreter, run glirc.lua in current directory,
 * register the first returned result of running the file as
 * the callback for message processing.
 *
 */
static void *start(void) {
        lua_State *L = luaL_newstate();
        if (L == NULL) return NULL;

        luaL_openlibs(L);

        int res = luaL_dofile(L, "glirc.lua");
        if (!res) {
          lua_setfield(L, LUA_REGISTRYINDEX, PROC_KEY);
        }

        lua_settop(L, 0);

        return L;
}

/* Shutdown the Lua interpreter
 *
 * [-0, +0, -]
 */
static void stop(void * S) {
        if (S == NULL) return;

        lua_State *L = S;
        lua_close(L);
}

/* Push the string contained in s on the top of the stack
 *
 * [-0, +1, m]
 * */

static void push_glirc_string(lua_State *L, const struct glirc_string *s) {
        lua_pushlstring(L, s->str, s->len);
}

/* Push a table onto the top of the stack containing all of the fields
 * of the message struct
 *
 * [-0, +1, m]
 * */
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

        {
                const size_t nrec = 0, narr = msg->params_n;
                lua_createtable(L, narr, nrec);

                /* initialize table */
                for (size_t i = 0; i < narr; i++) {
                        push_glirc_string(L, &msg->params[i]);
                        lua_seti(L, -2, i+1);
                }
                lua_setfield(L,-2,"params");
        }

        { /* populate tags */
                const size_t nrec = msg->tags_n, narr = 0;
                lua_createtable(L, narr, nrec);

                /* initialize table */
                for (size_t i = 0; i < nrec; i++) {
                        push_glirc_string(L, &msg->tagkeys[i]);
                        push_glirc_string(L, &msg->tagvals[i]);
                        lua_rawset(L, -3);
                }
                lua_setfield(L,-2,"tags");
        }
}

static void process_message(void *glirc, void * S, const struct glirc_message *msg) {
        if (S == NULL) return;

        lua_State *L = S;

        (void)lua_getfield(L, LUA_REGISTRYINDEX, PROC_KEY);
        push_glirc_message(L, msg);
        (void)lua_pcall(L, 1, 0, 0);
}

struct glirc_extension extension = {
        .name            = "Lua",
        .major_version   = 1,
        .minor_version   = 0,
        .start           = start,
        .stop            = stop,
        .process_message = process_message
};
