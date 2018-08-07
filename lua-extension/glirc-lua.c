/// Extension API for glirc
// This module provides extension functionality for the glirc IRC client.
// Extensions are expected to be implemented as Lua modules that return
// a single table with callbacks that the client can dispatch events to.
// @module glirc
// @author Eric Mertens
// @license ISC
// @copyright Eric Mertens 2018

#include <string.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <libgen.h>

#include "glirc-api.h"

static char glirc_callback_module_key;

#define MAJOR 1
#define MINOR 0

/* Helper
 * Pushes the string represented by the argument to the top of the stack
 */
static void get_glirc_string(lua_State *L, int i, struct glirc_string *s)
{
        s->str = lua_tolstring(L, i, &s->len);
}

static inline struct glirc *get_glirc(lua_State *L)
{
        struct glirc *G;
        memcpy(&G, lua_getextraspace(L), sizeof(G));
        return G;
}

/***
Send an IRC command on a connected network. Message tags are ignored
when sending a message.
@function send_message
@tparam message message IRC message to send on network
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

        if (lua_getfield(L, 1, "command") != LUA_TSTRING) {
                luaL_error(L, "bad command field");
        }
        get_glirc_string(L, -1, &msg.command);

        if (lua_getfield(L, 1, "network") != LUA_TSTRING) {
                luaL_error(L, "bad network field");
        }
        get_glirc_string(L, -1, &msg.network);

        if (lua_getfield(L, 1, "params") != LUA_TTABLE) {
                luaL_error(L, "bad params field");
        }

        lua_Integer n = luaL_len(L,-1);

        if (n > 15) luaL_error(L, "too many command parameters");

        struct glirc_string params[n];
        msg.params   = params;
        msg.params_n = n;

        for (lua_Integer i = 1; i <= n; i++) {
                if (lua_geti(L, 4, i) != LUA_TSTRING) {
                        luaL_error(L, "bad command parameter[%d]", i);
                }
                get_glirc_string(L, -1, &params[i-1]);
        }

        if (glirc_send_message(get_glirc(L), &msg)) {
                luaL_error(L, "failure in client");
        }

        return 0;
}

/* Push path to glirc.lua which should be in the same directory
 * as the given path to the extension shared library.
 *
 * Returns 0 on success, non-zero on failure.
 */
void push_scriptname(lua_State *L, const char *path) {
        char path_copy[strlen(path)+1];
        strcpy(path_copy, path);

        char *dir_part = dirname(path_copy);
        lua_pushfstring(L, "%s/glirc.lua", dir_part);
}

/***
Print a message to the client console
@function print
@tparam string message Message to print to console
*/
static int glirc_lua_print(lua_State *L)
{
        size_t msglen = 0;
        const char *msg = luaL_checklstring(L, 1, &msglen);
        luaL_checktype(L, 2, LUA_TNONE);

        glirc_print(get_glirc(L), NORMAL_MESSAGE, msg, msglen);
        return 0;
}

/***
Print an error message to the client console
@function error
@tparam string message Message to print to console
*/
static int glirc_lua_error(lua_State *L)
{
        size_t msglen = 0;
        const char *msg = luaL_checklstring(L, 1, &msglen);
        luaL_checktype(L, 2, LUA_TNONE);

        glirc_print(get_glirc(L), ERROR_MESSAGE, msg, msglen);
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
                lua_rawseti(L, -2, i+1);
        }
        glirc_free_strings(list);
}

/***
Generate a list of names of connected networks.
@function list_networks
@treturn {string,...} A table of network names
*/
static int glirc_lua_list_networks(lua_State *L)
{
        luaL_checktype(L, 1, LUA_TNONE);

        char **networks = glirc_list_networks(get_glirc(L));
        if (networks == NULL) { luaL_error(L, "client failure"); }

        import_string_array(L, networks);

        return 1;
}

/***
List the connected channels for a given network
@function list_channels
@tparam string network Network name
@treturn {string,...} A table of channel names
*/
static int glirc_lua_list_channels(lua_State *L)
{
        size_t network_len;
        const char *network;
        network = luaL_checklstring(L, 1, &network_len);
        luaL_checktype(L, 2, LUA_TNONE);

        char **channels = glirc_list_channels(get_glirc(L), network, network_len);
        if (channels == NULL) { luaL_error(L, "no such network"); }

        import_string_array(L, channels);

        return 1;
}

/***
List the users in a channel
@function list_channel_users
@tparam string network Network name
@tparam string channel Channel name
@treturn {string,...} A table of nicknames
*/
static int glirc_lua_list_channel_users(lua_State *L)
{
        size_t network_len, channel_len;
        const char *network, *channel;
        network = luaL_checklstring(L, 1, &network_len);
        channel = luaL_checklstring(L, 2, &channel_len);
        luaL_checktype(L, 3, LUA_TNONE);

        char **users = glirc_list_channel_users
                        (get_glirc(L), network, network_len,
                                       channel, channel_len);
        if (users == NULL) { luaL_error(L, "no such channel"); }

        import_string_array(L, users);

        return 1;
}

/***
Determine the services account for a given nickname
@function user_account
@tparam string network Network name
@tparam string nick    User nickname
@treturn ?string Account name if known, otherwise nil
*/
static int glirc_lua_user_account(lua_State *L)
{
        size_t netlen = 0, nicklen = 0;
        const char *net = luaL_checklstring(L, 1, &netlen);
        const char *nick = luaL_checklstring(L, 2, &nicklen);
        luaL_checktype(L, 3, LUA_TNONE);

        char *acct = glirc_user_account(get_glirc(L), net, netlen, nick, nicklen);
        lua_pushstring(L, acct);
        glirc_free_string(acct);

        return 1;
}

/***
Return the mode sigils for a user on a channel (e.g. + or @)
@function user_channel_modes
@tparam string network Network name
@tparam string channel Channel name
@tparam string nick User nickname
@treturn ?string Sigils if on channel, nil otherwise
*/
static int glirc_lua_user_channel_modes(lua_State *L)
{
        size_t netlen = 0, chanlen = 0, nicklen = 0;
        const char *net = luaL_checklstring(L, 1, &netlen);
        const char *chan = luaL_checklstring(L, 2, &chanlen);
        const char *nick = luaL_checklstring(L, 3, &nicklen);
        luaL_checktype(L, 4, LUA_TNONE);

        char *sigils = glirc_user_channel_modes(get_glirc(L), net, netlen, chan, chanlen, nick, nicklen);
        lua_pushstring(L, sigils);
        glirc_free_string(sigils);

        return 1;
}

/***
Return the client's nickname on a particular network
@function my_nick
@tparam string network Network name
@treturn ?string Client user's nickname if connected, otherwise nil
*/
static int glirc_lua_my_nick(lua_State *L)
{
        size_t netlen = 0;
        const char *net = luaL_checklstring(L, 1, &netlen);
        luaL_checktype(L, 2, LUA_TNONE);

        char *nick = glirc_my_nick(get_glirc(L), net, netlen);
        lua_pushstring(L, nick);
        glirc_free_string(nick);

        return 1;
}

/***
Mark a client window seen cleaning the unread message counter. The
window name should be either a channel name or a user nickname.
@function mark_seen
@tparam string network Network name
@tparam string channel Window name
*/
static int glirc_lua_mark_seen(lua_State *L)
{
        size_t network_len, channel_len;
        const char *network, *channel;
        network = luaL_optlstring(L, 1, NULL, &network_len);
        channel = luaL_optlstring(L, 2, NULL, &channel_len);
        luaL_checktype(L, 3, LUA_TNONE);

        glirc_mark_seen(get_glirc(L), network, network_len,
                                      channel, channel_len);
        return 0;
}

/***
Clear all message from a client window. The window name should be
either a channel name or a user nickname.
@function clear_window
@tparam string network Network name
@tparam string channel Window name
*/
static int glirc_lua_clear_window(lua_State *L)
{
        size_t network_len, channel_len;
        const char *network, *channel;
        network = luaL_optlstring(L, 1, NULL, &network_len);
        channel = luaL_optlstring(L, 2, NULL, &channel_len);
        luaL_checktype(L, 3, LUA_TNONE);

        glirc_clear_window(get_glirc(L), network, network_len,
                                         channel, channel_len);
        return 0;
}

/***
Find the name of the currently focused window
either a channel name or a user nickname.
@function current_focus
@treturn ?string Network name
@treturn ?string Target name
*/
static int glirc_lua_current_focus(lua_State *L)
{
        luaL_checktype(L, 1, LUA_TNONE);

        size_t network_len = 0, target_len = 0;
        char *network = NULL, *target = NULL;
        glirc_current_focus(get_glirc(L), &network, &network_len,
                                          &target,  &target_len);

        lua_pushlstring(L, network, network_len);
        lua_pushlstring(L, target, target_len);

        glirc_free_string(network);
        glirc_free_string(target);

        return 2;
}

/***
Determine if we are sure that the given user on the given network is
currently connected.
@function is_logged_on
@tparam string network Network name
@tparam string nickname Nickname
@treturn boolean User known to be connected
*/
static int glirc_lua_is_logged_on(lua_State *L)
{
        size_t network_len = 0, target_len = 0;
        const char *network = NULL, *target = NULL;
        network = luaL_checklstring(L, 1, &network_len);
        target  = luaL_checklstring(L, 2, &target_len);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = glirc_is_logged_on(get_glirc(L), network, network_len,
                                                   target, target_len);
        lua_pushboolean(L, res);

        return 1;
}

/***
Determine if the given target is the name of a channel
currently connected.
@function is_channel
@tparam string network Network name
@tparam string target Target name
@treturn boolean Target is a channel name
*/
static int glirc_lua_is_channel(lua_State *L)
{
        size_t network_len = 0, target_len = 0;
        const char *network = NULL, *target = NULL;
        network = luaL_checklstring(L, 1, &network_len);
        target  = luaL_checklstring(L, 2, &target_len);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = glirc_is_channel(get_glirc(L), network, network_len,
                                                 target, target_len);
        lua_pushboolean(L, res);

        return 1;
}

/***
Case-insensitive comparison of two identifiers using IRC case map.
Return -1 when first identifier is "greater than" the second.
Return 0 when first identifier is "equal to" the second.
Return 1 when first identifier is "less than" the second.
@function identifier_cmp
@tparam string identifier1 First identifier
@tparam string identifier2 Second identifier
@treturn integer Comparison result
*/
static int glirc_lua_identifier_cmp(lua_State *L)
{
        size_t str1_len, str2_len;
        const char *str1, *str2;
        str1 = luaL_checklstring(L, 1, &str1_len);
        str2 = luaL_checklstring(L, 2, &str2_len);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = glirc_identifier_cmp(str1, str1_len, str2, str2_len);
        lua_pushinteger(L, res);

        return 1;
}

void new_formatting_table(lua_State *L) {
        lua_createtable(L, 0, 21);

        lua_pushliteral(L, "\x0f");
        lua_setfield(L, -2, "reset");

        lua_pushliteral(L, "\x1f"); \
        lua_setfield(L, -2, "underline");
        lua_pushliteral(L, "\x1d"); \
        lua_setfield(L, -2, "italic");
        lua_pushliteral(L, "\x02"); \
        lua_setfield(L, -2, "bold");
        lua_pushliteral(L, "\x16"); \
        lua_setfield(L, -2, "reverse");

#define COLOR(name, code) \
        lua_pushstring(L, "\x03" code); \
        lua_setfield(L, -2, name);

        COLOR("white"      , "00")
        COLOR("black"      , "01")
        COLOR("blue"       , "02")
        COLOR("green"      , "03")
        COLOR("red"        , "04")
        COLOR("brown"      , "05")
        COLOR("purple"     , "06")
        COLOR("orange"     , "07")
        COLOR("yellow"     , "08")
        COLOR("light_green", "09")
        COLOR("cyan"       , "10")
        COLOR("light_cyan" , "11")
        COLOR("light_blue" , "12")
        COLOR("pink"       , "13")
        COLOR("gray"       , "14")
        COLOR("light_gray" , "15")
#undef COLOR
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
  , { "user_account"      , glirc_lua_user_account       }
  , { "user_channel_modes", glirc_lua_user_channel_modes }
  , { "mark_seen"         , glirc_lua_mark_seen          }
  , { "clear_window"      , glirc_lua_clear_window       }
  , { "current_focus"     , glirc_lua_current_focus      }
  , { "is_logged_on"      , glirc_lua_is_logged_on       }
  , { "is_channel"        , glirc_lua_is_channel         }
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

        new_formatting_table(L);
        lua_setfield   (L, -2, "format");

        lua_setglobal(L, "glirc");
}


/* This function actually initialized the Lua state and
 * runs the user's script. It is allowed to raise errors
 * which will be caught by the use of lua_pcall in start.
 */
static int initialize_lua(lua_State *L)
{
        // Validate dynamic library Lua version
        luaL_checkversion(L);

        // Validate function arguments
        const char *scriptpath = luaL_checkstring(L, 1); // STACK: scriptpath
        luaL_checktype(L, 2, LUA_TNONE);

        // Load user script
        if (luaL_loadfile(L, scriptpath)) { // STACK: scriptpath script
                lua_error(L);
        }

        // Store script path in arg table
        lua_newtable(L);         // STACK: scriptpath script arg
        lua_rotate(L, 1, -1);    // STACK: script arg scriptpath
        lua_rawseti(L, -2, 0);   // STACK: script arg
        lua_setglobal(L, "arg"); // STACK: script

        // Initialize libraries
        luaL_openlibs(L);
        glirc_install_lib(L);

        // Execute user script
        lua_call(L, 0, 1);       // STACK: module

        lua_rawsetp(L, LUA_REGISTRYINDEX, &glirc_callback_module_key); // STACK:

        return 0;
}

/* Start the Lua interpreter, run glirc.lua in current directory,
 * register the first returned result of running the file as
 * the callback for message processing.
 *
 */
static void *start(struct glirc *G, const char *path)
{
        if (LUA_EXTRASPACE < sizeof(struct glirc *)) {
                const char * const err = "Lua extraspace too small";
                glirc_print(G, ERROR_MESSAGE, err, strlen(err));
                return NULL;
        }

        lua_State *L = luaL_newstate();
        if (L == NULL) {
                const char * const err = "Failed to allocate Lua interpreter";
                glirc_print(G, ERROR_MESSAGE, err, strlen(err));
                return NULL;
        }

        // Store glirc token in extra space, used for re-entry into glirc
        memcpy(lua_getextraspace(L), &G, sizeof(G));

        lua_pushcfunction(L, initialize_lua);
        push_scriptname(L, path);

        if (lua_pcall(L, 1, 0, 0)) {
                size_t msglen = 0;
                const char *msg = lua_tolstring(L, -1, &msglen);
                glirc_print(G, ERROR_MESSAGE, msg, msglen);

                lua_close(L);
                L = NULL;
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
 * of the chat struct
 *
 * [-0, +1, m]
 * */
static void push_glirc_chat(lua_State *L, const struct glirc_chat *chat)
{
        lua_createtable(L, 0, 3);

        push_glirc_string(L, &chat->network);
        lua_setfield(L,-2,"network");

        push_glirc_string(L, &chat->target);
        lua_setfield(L,-2,"target");

        push_glirc_string(L, &chat->message);
        lua_setfield(L,-2,"message");
}

/* Push a table onto the top of the stack containing all of the fields
 * of the command struct
 *
 * [-0, +1, m]
 * */
static void push_glirc_command(lua_State *L, const struct glirc_command *cmd)
{
        lua_createtable(L, 0, 1);

        push_glirc_string(L, &cmd->command);
        lua_setfield(L,-2,"command");
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
{       int n = lua_gettop(L);                                         // name args...
        lua_rawgetp(L, LUA_REGISTRYINDEX, &glirc_callback_module_key); // name args... ext
        lua_insert(L, 1);                                              // ext name args...
        lua_rotate(L, 2, -1);                                          // ext args... name
        if (lua_gettable(L, 1) != LUA_TNIL) {                          // ext args... callback/nil
            lua_insert(L, 1);                                          // callback ext args...
            lua_call(L, n, 1);                                         // result
        }
        return 1; // result/nil
}

static enum process_result callback(struct glirc *G, lua_State *L, int nargs)
{
                                               // STACK: name arguments...
        lua_pushcfunction(L, callback_worker); // STACK: name arguments... worker
        lua_insert(L, 1);                      // STACK: worker name arguments...
        int res = lua_pcall(L, 1+nargs, 1, 0); // STACK: result

        if (res != LUA_OK) {
                size_t msglen = 0;
                const char *msg = lua_tolstring(L, -1, &msglen);
                glirc_print(G, ERROR_MESSAGE, msg, msglen);
                lua_settop(L, 0); // discard error message
                return PASS_MESSAGE;
        }

        res = lua_toboolean(L, 1);
        lua_settop(L, 0);
        return res ? DROP_MESSAGE : PASS_MESSAGE;
}

static void stop_entrypoint(struct glirc *G, void *L)
{
        if (L == NULL) return;
        lua_pushliteral(L, "stop");
        callback(G, L, 0);
        lua_close(L);
}

static enum process_result message_entrypoint(struct glirc *G, void *L, const struct glirc_message *msg)
{
        if (L == NULL) return PASS_MESSAGE;
        lua_pushliteral(L, "process_message");
        push_glirc_message(L, msg);
        return callback(G, L, 1);
}

static enum process_result chat_entrypoint(struct glirc *G, void *L, const struct glirc_chat *chat)
{
        if (L == NULL) return PASS_MESSAGE;
        lua_pushliteral(L, "process_chat");
        push_glirc_chat(L, chat);
        return callback(G, L, 1);
}

static void command_entrypoint(struct glirc *G, void *L, const struct glirc_command *cmd)
{
        if (L == NULL) return;
        lua_pushliteral(L, "process_command");
        push_glirc_command(L, cmd);
        callback(G, L, 1);
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
@see send_message
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
