/***
Callback registration interface

A client script should implement this module interface to handle
callbacks from the client. The script will be executed with the scripts
arguments passed in the 'arg' table. All standard Lua libraries are
registered.  Client interaction is available through the `glirc` lirary.

@module extension
@author Eric Mertens
@license ISC
@copyright Eric Mertens 2018
*/

#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <string.h>
#include <libgen.h>

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "glirc-api.h"
#include "glirc-marshal.h"
#include "glirc-lib.h"

/* The Lua extraspace is used to store the glirc client
 * handle for reentry into the client
 */
static_assert(LUA_EXTRASPACE >= sizeof(struct glirc *),
              "Lua extraspace too small");

/* This global variable provides a unique key for storing
 * information in the Lua registry.
 */
static char glirc_callback_module_key;

/* Push path to glirc.lua which should be in the same directory
 * as the given path to the extension shared library.
 *
 * Returns 0 on success, non-zero on failure.
 */
static void push_scriptname
  (lua_State *L,
   const char *path,
   const struct glirc_string *args,
   size_t args_n)
{
        // When no arguments are provided, default to glirc.lua
        if (args_n == 0) {
                char *path_copy = strdup(path);
                if (NULL == path_copy) {
                    luaL_error(L, "allocation failure");
                }
                const char *dir_part = dirname(path_copy);
                lua_pushfstring(L, "%s/glirc.lua", dir_part);
                free(path_copy);
        } else {
                char * script = glirc_resolve_path
                                  (get_glirc(L), args[0].str, args[0].len);
                lua_pushstring(L, script);
                glirc_free_string(script);
        }
}

static void push_args_table(lua_State *L, const struct glirc_string *args, size_t args_len) {
        lua_createtable(L, args_len > 1 ? args_len - 1 : 0, 1);
        for (size_t i = 1; i < args_len; i++) {
                push_glirc_string(L, args+i);
                lua_rawseti(L, -2, i);
        }
}

/* This function actually initialized the Lua state and
 * runs the user's script. It is allowed to raise errors
 * which will be caught by the use of lua_pcall in start.
 */
static int initialize_lua(lua_State *L)
{
        // Validate dynamic library Lua version
        luaL_checkversion(L);

        const char * const path                = lua_touserdata(L, 1);
        const struct glirc_string * const args = lua_touserdata(L, 2);
        const lua_Integer args_len             = lua_tointeger (L, 3);

        lua_settop(L, 0);

        push_args_table(L, args, args_len);
        push_scriptname(L, path, args, args_len);
        const char *scriptpath = lua_tostring(L, 2);

        // Load user script
        if (luaL_loadfile(L, scriptpath)) { // STACK: arg scriptpath script
                lua_error(L);
        }
        lua_insert(L, 1);        // STACK script arg scriptpath
        lua_rawseti(L, -2, 0);   // STACK: script arg
        lua_setglobal(L, "arg"); // STACK: script

        // Initialize libraries
        luaL_openlibs(L);
        glirc_install_lib(L);

        // Execute user script
        lua_call(L, 0, 1);       // STACK: module

        lua_rawsetp(L, LUA_REGISTRYINDEX, &glirc_callback_module_key);
        // STACK:

        return 0;
}

/* Start the Lua interpreter, run glirc.lua in current directory,
 * register the first returned result of running the file as
 * the callback for message processing.
 *
 */
static void *start
  (struct glirc *G,
   const char *path,
   const struct glirc_string *args,
   size_t args_len)
{
        const char * err;
        lua_State *L = NULL;

        L = luaL_newstate();
        if (L == NULL) {
                err = "Failed to allocate Lua interpreter";
                goto cleanup;
        }

        // Store glirc token in extra space, used for re-entry into glirc
        memcpy(lua_getextraspace(L), &G, sizeof(G));

        lua_pushcfunction    (L, initialize_lua);
        lua_pushlightuserdata(L, (void*)path);
        lua_pushlightuserdata(L, (void*)args);
        lua_pushinteger      (L, args_len);

        if (lua_pcall(L, 3, 0, 0)) {
                err = lua_tostring(L, -1);
                goto cleanup;
        }

        return L;

cleanup:
        glirc_print(G, ERROR_MESSAGE, err, strlen(err));
        // close *after* printing error. Error could be on stack
        if (L) lua_close(L);
        return NULL;
}


static int callback_worker(lua_State *L)
{       int n = lua_gettop(L);                                         // args... name
        lua_rawgetp(L, LUA_REGISTRYINDEX, &glirc_callback_module_key); // args... name ext
        lua_insert(L, 1);                                              // ext name args... name
        if (lua_gettable(L, 1) != LUA_TNIL) {                          // ext args... callback/nil
            lua_insert(L, 1);                                          // callback ext args...
            lua_call(L, n, 1);                                         // result
        }
        return 1; // result/nil
}

static enum process_result
callback (lua_State *L, const char *name, int nargs)
{
                                               // STACK: arguments...
        lua_pushcfunction(L, callback_worker); // STACK: arguments... worker
        lua_insert(L, -nargs-1);               // STACK: worker arguments...
        lua_pushstring(L, name);               // STACK: worker arguments... name
        int res = lua_pcall(L, 1+nargs, 1, 0); // STACK: result

        if (res != LUA_OK) {
                size_t msglen = 0;
                const char *msg = lua_tolstring(L, -1, &msglen);
                glirc_print(get_glirc(L), ERROR_MESSAGE, msg, msglen);
                lua_pop(L, 1); // discard error message
                return PASS_MESSAGE;
        }

        res = lua_toboolean(L, -1);
        lua_pop(L, 1);
        return res ? DROP_MESSAGE : PASS_MESSAGE;
}

/***
Callback used by client when unloading the Lua extension.
@function stop
@tparam extension self Extension module
*/
static void stop_entrypoint(void *L)
{
        if (L == NULL) return;
        callback(L, "stop", 0);
        lua_close(L);
}

/***
Callback used when client receives message from the server.
@function process_message
@tparam extension self Extension module
@tparam message message Message received
@treturn bool Return true to drop this message
*/
static enum process_result
message_entrypoint
  (void *L,
   const struct glirc_message *msg)
{
        if (L == NULL) return PASS_MESSAGE;
        push_glirc_message(L, msg);
        return callback(L, "process_message", 1);
}

/***
Callback used when client submits a chat message.
@function process_chat
@tparam extension self Extension module
@tparam chat chat Chat information table
@treturn bool Return true to drop this message
*/
static enum process_result
chat_entrypoint
  (void *L,
   const struct glirc_chat *chat)
{
        if (L == NULL) return PASS_MESSAGE;
        push_glirc_chat(L, chat);
        return callback(L, "process_chat", 1);
}

/***
Callback used when client submits an /extension Lua command
@function process_command
@tparam extension self Extension module
@tparam string command Command text
*/
static void
command_entrypoint
  (void *L,
   const struct glirc_command *cmd)
{
        if (L == NULL) return;
        push_glirc_command(L, cmd);
        callback(L, "process_command", 1);
}

/***
When scripting glirc, the glirc.lua file should return a table with these
fields. Any omitted field will be ignored during its corresponding event.

process_message is called with a message argument.

process_command is called with a command argument.

@table extension
@tfield func process_message Function called to process a message
@tfield func process_command Function called to process a client /extension command
@tfield func process_chat Function called to process client sending a chat message
@tfield func stop Function called when unloading this extension
*/

/***
Table used with process_command
@table command
@tfield string command Argument to the /extension client command
*/

/***
Table used with process_chat
@table chat
@tfield string network Network name
@tfield string target Window name
@tfield string message Message body
*/

/***
Table used with send_message and process_message.
@table message
@tfield {[string]=string,...} tags Message tags
@tfield string network Network name
@tfield prefix prefix Sender information
@tfield string command IRC command
@tfield {string,...} params Command parameters
*/

/***
Prefix information for messages being sent or received
@table prefix
@tfield string nick Nickname
@tfield string user Username
@tfield string host Hostname
*/

struct glirc_extension extension = {
        .name            = "Lua",
        .major_version   = MAJOR,
        .minor_version   = MINOR,
        .start           = start,
        .stop            = stop_entrypoint,
        .process_message = message_entrypoint,
        .process_command = command_entrypoint,
        .process_chat    = chat_entrypoint
};
