#include <string.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <libgen.h>

#include "glirc-api.h"

#define CALLBACK_MODULE_KEY "glirc-callback-module"
#define MAJOR 1
#define MINOR 0

/* Helper
 * Pushes a the string represented by the argument to the top of the stack
 */
static void get_glirc_string(lua_State *L, int i, struct glirc_string *s)
{
        s->str = lua_tolstring(L, i, &s->len);
}

static inline void * get_glirc(lua_State *L)
{
        void * glirc;
        memcpy(&glirc, lua_getextraspace(L), sizeof(glirc));
        return glirc;
}

/* Lua Function:
 * Arguments: Message (table with .command (string) .network (string) .params (array of string))
 * Returns:
 */
static int glirc_lua_send_message(lua_State *L)
{
        /* This module is careful to leave strings on the stack
         * while it is adding them to the message struct.
         *
         * Stack layout:
         * 1. Message table
         * 2. Command string
         * 3. Network string
         * 4. Params table
         * 5... params strings
         */

        luaL_checkany(L, 1);
        luaL_checktype(L, 2, LUA_TNONE);

        struct glirc_message msg = { {0} };

        lua_getfield(L, 1, "command");
        get_glirc_string(L, -1, &msg.command);

        lua_getfield(L, 1, "network");
        get_glirc_string(L, -1, &msg.network);

        lua_getfield(L, 1, "params");
        lua_len(L, -1);
        lua_Integer n = lua_tointeger(L,-1);
        lua_settop(L, -2);

        if (n > 15) luaL_error(L, "too many command parameters");

        struct glirc_string params[n];
        msg.params   = params;
        msg.params_n = n;

        for (int i = 0; i < n; i++) {
                lua_geti(L, 4, i+1);
                get_glirc_string(L, -1, &params[i]);
        }

        if (glirc_send_message(get_glirc(L), &msg)) {
                luaL_error(L, "failure in client");
        }

        return 0;
}

/* Populate scriptpath by computing the filename glirc.lua
 * in the same directory as the file in libpath.
 *
 * scriptpath must be a character array able to hold up to
 * PATH_MAX characters.
 */
int compute_script_path(const char *libpath, char *scriptpath)
{
        if (libpath == NULL) { return -1; }
        if (strlen(libpath) >= PATH_MAX) { return -2; }

        /* dirname is documented to be allowed to alter the input string
         * so first it's copied into the output buffer */
        strcpy(scriptpath, libpath);
        char * dirpart = dirname(scriptpath);
        if (dirpart == NULL) { return -3; }

        int res = snprintf(scriptpath, PATH_MAX, "%s/glirc.lua", dirpart);
        if (res < 0 || res >= PATH_MAX) { return -4; }

        return 0;
}

/* Lua Function:
 * Arguments: Message (string)
 * Returns:
 */
static int glirc_lua_print(lua_State *L)
{
        size_t len;
        const char *str = luaL_checklstring(L, 1, &len);
        luaL_checktype(L, 2, LUA_TNONE);

        glirc_print(get_glirc(L), NORMAL_MESSAGE, str, len);
        return 0;
}

/* Lua Function:
 * Arguments: Message (string)
 * Returns:
 */
static int glirc_lua_error(lua_State *L)
{
        size_t len;
        const char *str = luaL_checklstring(L, 1, &len);
        luaL_checktype(L, 2, LUA_TNONE);

        glirc_print(get_glirc(L), ERROR_MESSAGE, str, len);
        return 0;
}

/* Helper function
 * Returns: Array of strings
 * Import the given array of strings, free the strings and the list
 */
static void import_string_array(lua_State *L, char **list)
{
        lua_newtable(L);
        for (int i = 0; list[i] != NULL; i++) {
                lua_pushstring(L, list[i]);
                free(list[i]);
                lua_rawseti(L, -2, i+1);
        }
        free(list);
}

/* Lua Function:
 * Arguments:
 * Returns: Networks (array of string)
 */
static int glirc_lua_list_networks(lua_State *L)
{
        luaL_checktype(L, 1, LUA_TNONE);

        char **networks = glirc_list_networks(get_glirc(L));
        if (networks == NULL) { luaL_error(L, "client failure"); }

        import_string_array(L, networks);

        return 1;
}

/* Lua Function:
 * Arguments: Network (string)
 * Returns: Channels (array of string)
 */
static int glirc_lua_list_channels(lua_State *L)
{
        size_t networkLen;
        const char *network = luaL_checklstring(L, 1, &networkLen);
        luaL_checktype(L, 2, LUA_TNONE);

        char **channels = glirc_list_channels(get_glirc(L), network, networkLen);
        if (channels == NULL) { luaL_error(L, "no such network"); }

        import_string_array(L, channels);

        return 1;
}

/* Lua Function:
 * Arguments: Network (string), Channel (string)
 * Returns: Users (array of string)
 */
static int glirc_lua_list_channel_users(lua_State *L)
{
        size_t networkLen, channelLen;
        const char *network = luaL_checklstring(L, 1, &networkLen);
        const char *channel = luaL_checklstring(L, 2, &channelLen);
        luaL_checktype(L, 3, LUA_TNONE);

        char **users = glirc_list_channel_users
                                (get_glirc(L), network, networkLen,
                                               channel, channelLen);
        if (users == NULL) { luaL_error(L, "no such channel"); }

        import_string_array(L, users);

        return 1;
}

/* Lua Function:
 * Arguments: Network (string)
 * Returns: Nick (string)
 */
static int glirc_lua_my_nick(lua_State *L)
{
        size_t networkLen;
        const char *network = luaL_checklstring(L, 1, &networkLen);
        luaL_checktype(L, 2, LUA_TNONE);

        char *nick = glirc_my_nick(get_glirc(L), network, networkLen);
        if (nick == NULL) { luaL_error(L, "no such network"); }
        lua_pushstring(L, nick);
        free(nick);

        return 1;
}

/* Lua Function:
 * Arguments: Identifier (string), Identifier (string)
 * Returns: Comparison (integer)
 */
static int glirc_lua_identifier_cmp(lua_State *L)
{
        size_t n1, n2;
        const char *str1 = luaL_checklstring(L, 1, &n1);
        const char *str2 = luaL_checklstring(L, 2, &n2);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = glirc_identifier_cmp(str1, n1, str2, n2);
        lua_pushinteger(L, res);

        return 1;
}

static luaL_Reg glirc_lib[] =
  { { "send_message"      , glirc_lua_send_message       }
  , { "print"             , glirc_lua_print              }
  , { "error"             , glirc_lua_error              }
  , { "identifier_cmp"    , glirc_lua_identifier_cmp     }
  , { "list_networks"     , glirc_lua_list_networks      }
  , { "list_channels"     , glirc_lua_list_channels      }
  , { "list_channel_users", glirc_lua_list_channel_users }
  , { "my_nick"           , glirc_lua_my_nick            }
  , { NULL                , NULL                         }
  };

/* Helper function
 * Installs the 'glirc' library into the global environment
 * No stack effect
 */
static void glirc_install_lib(lua_State *L)
{
        luaL_newlib(L, glirc_lib);

        /* add version table */
        lua_createtable(L, 0, 2);
        lua_pushinteger(L, MAJOR);
        lua_setfield   (L, -2, "major");
        lua_pushinteger(L, MINOR);
        lua_setfield   (L, -2, "minor");
        lua_setfield   (L, -2, "version");

        lua_setglobal(L, "glirc");
}

/* Start the Lua interpreter, run glirc.lua in current directory,
 * register the first returned result of running the file as
 * the callback for message processing.
 *
 */
static void *start(void *glirc, const char *path)
{
        char scriptpath[PATH_MAX];
        if (compute_script_path(path, scriptpath)) {
                return NULL;
        }

        lua_State *L = luaL_newstate();
        if (L == NULL) return NULL;
        memcpy(lua_getextraspace(L), &glirc, sizeof(glirc));


        luaL_openlibs(L);
        glirc_install_lib(L);

        if (luaL_dofile(L, scriptpath)) {
                size_t len;
                const char *msg = lua_tolstring(L, -1, &len);
                glirc_print(glirc, ERROR_MESSAGE, msg, len);

                lua_close(L);
                L = NULL;
        } else {
                lua_setfield(L, LUA_REGISTRYINDEX, CALLBACK_MODULE_KEY);
                lua_settop(L, 0);
        }

        return L;
}

/* Push the string contained in s on the top of the stack
 *
 * [-0, +1, m]
 * */

static void push_glirc_string(lua_State *L, const struct glirc_string *s)
{
        lua_pushlstring(L, s->str, s->len);
}

/* Push a table onto the top of the stack containing all of the fields
 * of the message struct
 *
 * [-0, +1, m]
 * */
static void push_glirc_message(lua_State *L, const struct glirc_message *msg)
{
        lua_createtable(L, 0, 5);

        push_glirc_string(L, &msg->network);
        lua_setfield(L,-2,"network");

        lua_createtable(L, 0, 3);
        push_glirc_string(L, &msg->prefix_nick);
        lua_setfield(L,-2,"nick");
        push_glirc_string(L, &msg->prefix_user);
        lua_setfield(L,-2,"user");
        push_glirc_string(L, &msg->prefix_host);
        lua_setfield(L,-2,"host");
        lua_setfield(L,-2,"prefix");

        push_glirc_string(L, &msg->command);
        lua_setfield(L,-2,"command");

        { /* populate params */
                const int nrec = 0, narr = msg->params_n;
                lua_createtable(L, narr, nrec);

                /* initialize table */
                for (int i = 0; i < narr; i++) {
                        push_glirc_string(L, &msg->params[i]);
                        lua_rawseti(L, -2, i+1);
                }
                lua_setfield(L,-2,"params");
        }

        { /* populate tags */
                const int nrec = msg->tags_n, narr = 0;
                lua_createtable(L, narr, nrec);

                /* initialize table */
                for (int i = 0; i < nrec; i++) {
                        push_glirc_string(L, &msg->tagkeys[i]);
                        push_glirc_string(L, &msg->tagvals[i]);
                        lua_rawset(L, -3);
                }
                lua_setfield(L,-2,"tags");
        }
}

static int callback_worker(lua_State *L)
{       int n = lua_gettop(L);                                   // args... name
        lua_getfield(L, LUA_REGISTRYINDEX, CALLBACK_MODULE_KEY); // args... name ext
        lua_rotate(L, 1, 1);                                     // ext args... name
        lua_gettable(L, 1);                                      // ext args... callback
        lua_rotate(L, 1, 1);                                     // callback ext args...
        lua_call(L, n, 0);                                       //
        return 0;
}

static void callback(void *glirc, lua_State *L, const char *callback_name, int args)
{
        // remember glirc handle
        memcpy(lua_getextraspace(L), &glirc, sizeof(glirc));

                                               // STACK: arguments...
        lua_pushcfunction(L, callback_worker); // STACK: arguments... worker
        lua_rotate(L, 1, 1);                   // STACK: worker arguments...
        lua_pushstring(L, callback_name);      // STACK: worker arguments... name
        int res = lua_pcall(L, 1+args, 0, 0);  // STACK:

        if (res != LUA_OK) {
                size_t len;
                const char *msg = lua_tolstring(L, -1, &len);
                glirc_print(glirc, ERROR_MESSAGE, msg, len);
                lua_settop(L, 0); // discard error message
        }
}

static void stop_entrypoint(void *glirc, void *L)
{
        if (L == NULL) return;
        callback(glirc, L, "stop", 0);
        lua_close(L);
}

static void message_entrypoint(void *glirc, void *L, const struct glirc_message *msg)
{
        if (L == NULL) return;
        push_glirc_message(L, msg);
        callback(glirc, L, "process_message", 1);
}

static void command_entrypoint(void *glirc, void *L, const struct glirc_command *cmd)
{
        if (L == NULL) return;
        for (size_t i = 0; i < cmd->params_n; i++) {
                push_glirc_string(L, &cmd->params[i]);
        }
        callback(glirc, L, "process_command", cmd->params_n);
}

struct glirc_extension extension = {
        .name            = "Lua",
        .major_version   = MAJOR,
        .minor_version   = MINOR,
        .start           = start,
        .stop            = stop_entrypoint,
        .process_message = message_entrypoint,
        .process_command = command_entrypoint
};
