/***
Library for interacting with the client state

This module provides client-specific hooks back into the glirc client.
Through this library scripts can send messages, check modes, and more.

@module glirc
@author Eric Mertens
@license ISC
@copyright Eric Mertens 2018
*/

#include <assert.h>

#include <lauxlib.h>

#include "glirc-api.h"
#include "glirc-lib.h"
#include "glirc-marshal.h"

#include <stdatomic.h>

/***
Send an IRC command on a connected network. Message tags are ignored
when sending a message.
@function send_message
@tparam string network Network name
@tparam string command IRC Command
@tparam string ... Command parameters (max 15)
@raise `'too many parameters'` and `'client failure'`
@usage glirc.send_message('mynet', 'PRIVMSG', 'someone', 'Hello, Someone!')
*/
static int glirc_lua_send_message(lua_State *L)
{
        /* This function is careful to leave strings on the stack
         * while it is adding them to the message struct.
         */
        struct glirc_string params[15];
        struct glirc_message msg = { .params = params };

        msg.network.str = luaL_checklstring(L, 1, &msg.network.len);
        msg.command.str = luaL_checklstring(L, 2, &msg.command.len);

        int const n = lua_gettop(L) - 2;
        if (n > 15) luaL_error(L, "too many parameters");
        msg.params_n = n;

        // Array allocated on Lua stack automatically cleaned up on error

        for (int i = 0; i < n; i++) {
                params[i].str = luaL_checklstring(L, i+3, &params[i].len);
        }

        if (glirc_send_message(get_glirc(L), &msg)) {
                luaL_error(L, "client failure");
        }

        return 0;
}

/***
Add a message to a chat window as though it was said by the given user.
@function inject_chat
@tparam string network Network name
@tparam string source Message source
@tparam string target Target message window name
@tparam string message Chat message body
@raise `'client failure'`
@usage
glirc.inject_chat('mynet', 'nick!user@host', '#mychannel', 'An injected message')
glirc.inject_chat('mynet', 'script output', 'somenick', 'Script output text')
*/
static int glirc_lua_inject_chat(lua_State *L) {
        size_t netlen, srclen, tgtlen, msglen;
        const char *net = luaL_checklstring(L, 1, &netlen);
        const char *src = luaL_checklstring(L, 2, &srclen);
        const char *tgt = luaL_checklstring(L, 3, &tgtlen);
        const char *msg = luaL_checklstring(L, 4, &msglen);
        luaL_checktype(L, 5, LUA_TNONE);

        if (glirc_inject_chat(get_glirc(L),
               net, netlen, src, srclen, tgt, tgtlen, msg, msglen)) {
                luaL_error(L, "client failure");
        }
        return 0;
}

/***
Print a message to the client console
@function print
@tparam string message Message to print to console
@usage glirc.print('This shows up on the * window')
*/
static int glirc_lua_print_string(lua_State *L)
{
        size_t msglen;
        const char *msg = luaL_checklstring(L, 1, &msglen);
        luaL_checktype(L, 2, LUA_TNONE);

        glirc_print(get_glirc(L), NORMAL_MESSAGE, msg, msglen);
        return 0;
}

/***
Replacement for Lua's print function
@function print
@tparam string message Message to print to console
@usage print('This shows up on the * window')
*/
static int glirc_lua_print(lua_State *L)
{
        int n = lua_gettop(L);  /* number of arguments */

        luaL_Buffer b;
        luaL_buffinit(L, &b);

        lua_getglobal(L, "tostring");
        for (int i = 1; i <= n; i++) {
                lua_pushvalue(L, -1);  /* tostring */
                lua_pushvalue(L, i);   /* value to print */
                lua_call(L, 1, 1);

                if (!lua_isstring(L, -1)) {
                        return luaL_error(L, "'tostring' must return a string to 'print'");
                }
                if (i > 1) {
                        luaL_addchar(&b, '\t');
                }
                luaL_addvalue(&b);
        }

        luaL_pushresult(&b);
        size_t msglen;
        const char *msg = luaL_tolstring(L, -1, &msglen);
        glirc_print(get_glirc(L), NORMAL_MESSAGE, msg, msglen);

        return 0;
}

/***
Print an error message to the client console
@function error
@tparam string message Message to print to console
@usage glirc.error('This shows up on the * window')
*/
static int glirc_lua_error(lua_State *L)
{
        size_t msglen = 0;
        const char *msg = luaL_checklstring(L, 1, &msglen);
        luaL_checktype(L, 2, LUA_TNONE);

        glirc_print(get_glirc(L), ERROR_MESSAGE, msg, msglen);
        return 0;
}

/***
Generate a list of names of connected networks.
@function list_networks
@treturn {string,...} A table of network names
@raise `'client failure'`
@usage glirc.list_networks() --> { 'mynet' }
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
@raise `'no such network'`
@usage glirc.list_channels('mynet') --> { '#somechan', '#friends' }
*/
static int glirc_lua_list_channels(lua_State *L)
{
        size_t network_len;
        const char *network = luaL_checklstring(L, 1, &network_len);
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
@raise `'no such channel'`
@usage glirc.list_channel_users('mynet', '#somechan') --> { 'chatter', 'quietguy' }
*/
static int glirc_lua_list_channel_users(lua_State *L)
{
        size_t network_len, channel_len;
        const char *network = luaL_checklstring(L, 1, &network_len);
        const char *channel = luaL_checklstring(L, 2, &channel_len);
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
@treturn ?string Account name if known, otherwise `nil`
@usage glirc.user_account('mynet', 'somenick') --> 'anaccount'
*/
static int glirc_lua_user_account(lua_State *L)
{
        size_t netlen, nicklen;
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
@treturn ?string Sigils if on channel, `nil` otherwise
@usage glirc.user_channel_modes('mynet', '#somechan', 'an_op') --> '@'
*/
static int glirc_lua_user_channel_modes(lua_State *L)
{
        size_t netlen, chanlen, nicklen;
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
Return the modes for a channel
@function channel_modes
@tparam string network Network name
@tparam string channel Channel name
@treturn ?table Modes
@usage glirc.channel_modes('mynet', '#somechan') --> { k = 'thekey', n = '', t = '' }
*/
static int glirc_lua_channel_modes(lua_State *L)
{
        size_t netlen, chanlen;
        const char *net = luaL_checklstring(L, 1, &netlen);
        const char *chan = luaL_checklstring(L, 2, &chanlen);
        luaL_checktype(L, 3, LUA_TNONE);

        char **modes = glirc_channel_modes(get_glirc(L), net, netlen, chan, chanlen);

        if (modes) {
                lua_newtable(L);
                for (int i = 0; modes[i] != NULL; i++) {
                        char key[] = {0,0};
                        key[0] = modes[i][0];
                        lua_pushstring(L, &modes[i][1]);
                        lua_setfield(L, -2, key);
                }

                glirc_free_strings(modes);

                return 1;
        } else {
                return 0;
        }
}

/***
Return the masks for a channel.
Typical mask lists are `b` for bans, `q` for quiets, `e` for exempts, `I` for invex.
@function channel_masks
@tparam string network Network name
@tparam string channel Channel name
@tparam string mode Mode letter
@treturn ?table Masks
@usage glirc.channel_masks('mynet', '#somechan', 'b') --> { '*!*@spam.host', 'badguy!*@*' }
*/
static int glirc_lua_channel_masks(lua_State *L)
{
        size_t netlen, chanlen, modelen;
        const char *net = luaL_checklstring(L, 1, &netlen);
        const char *chan = luaL_checklstring(L, 2, &chanlen);
        const char *mode = luaL_checklstring(L, 3, &modelen);
        luaL_argcheck(L, modelen == 1, 3, "expected single mode character");
        luaL_checktype(L, 4, LUA_TNONE);

        char **masks = glirc_channel_masks(get_glirc(L), net, netlen, chan, chanlen, *mode);

        if (masks) {
                import_string_array(L, masks);
                return 1;
        } else {
                return 0;
        }
}

/***
Return the client's nickname on a particular network
@function my_nick
@tparam string network Network name
@treturn ?string Client user's nickname if connected, otherwise `nil`
@usage glirc.my_nick('mynet') --> 'mynick'
*/
static int glirc_lua_my_nick(lua_State *L)
{
        size_t netlen;
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
@usage
glirc.mark_seen('mynet', '#somechan') -- channel
glirc.mark_seen('mynet', 'chatter') -- direct message
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
@usage
glirc.clear_window('mynet', '#somechan') -- channel
glirc.clear_window('mynet', 'chatter') -- direct message
*/
static int glirc_lua_clear_window(lua_State *L)
{
        size_t network_len, channel_len;
        const char *network = luaL_optlstring(L, 1, NULL, &network_len);
        const char *channel = luaL_optlstring(L, 2, NULL, &channel_len);
        luaL_checktype(L, 3, LUA_TNONE);

        glirc_clear_window(get_glirc(L), network, network_len,
                                         channel, channel_len);
        return 0;
}

/***
Get currently focused window.

The client window `*` is identified by two `nil` values.

The network windows are identified by a network name and `nil` target.

The chat windows are identified by both a network name and a target name.

@function current_focus
@treturn ?string Network name
@treturn ?string Target name
@usage
glirc.current_focus() --> nil, nil
glirc.current_focus() --> 'mynet', nil
glirc.current_focus() --> 'mynet', '#somechan'
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
Set currently focused window.

The client window `*` is identified by two `nil` values.

The network windows are identified by a network name and `nil` target.

The chat windows are identified by both a network name and a target name.

@function set_focus
@tparam ?string Network name
@tparam ?string Target name
@usage
glirc.set_focus() --> client window
glirc.set_focus('mynet') --> Network window for mynet
glirc.set_focus('mynet', '#somechan') --> Chat window for #somechan
*/
static int glirc_lua_set_focus(lua_State *L)
{
        size_t network_len, target_len;
        const char *network = luaL_optlstring(L, 1, NULL, &network_len);
        const char *target  = luaL_optlstring(L, 2, NULL, &target_len);
        luaL_argcheck(L, network_len > 0 || target_len == 0, 2,
                      "target specified without a network");
        luaL_checktype(L, 3, LUA_TNONE);

        glirc_set_focus(get_glirc(L), network, network_len, target, target_len);
        return 0;
}

/***
Determine if we are sure that the given user on the given network is
currently connected.
@function is_logged_on
@tparam string network Network name
@tparam string nickname Nickname
@treturn boolean User known to be connected
@usage glirc.is_logged_on('mynet', 'chatter')
*/
static int glirc_lua_is_logged_on(lua_State *L)
{
        size_t network_len, target_len;
        const char *network = luaL_checklstring(L, 1, &network_len);
        const char *target  = luaL_checklstring(L, 2, &target_len);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = glirc_is_logged_on(get_glirc(L), network, network_len,
                                                   target, target_len);
        lua_pushboolean(L, res);

        return 1;
}

/***
Test if target identifies a channel.

This provides a network-specific test to determine if a target name
identifies a channel.  While most networks use `#` to prefix channel
names, there are other possibilities.

@function is_channel
@tparam string network Network name
@tparam string target Target name
@treturn boolean Target is a channel name
@usage
glirc.is_channel('mynet', 'chatter') --> false
glirc.is_channel('mynet', '#somechan') --> true
glirc.is_channel('mynet', '&somechan') --> true
*/
static int glirc_lua_is_channel(lua_State *L)
{
        size_t network_len, target_len;
        const char *network = luaL_checklstring(L, 1, &network_len);
        const char *target  = luaL_checklstring(L, 2, &target_len);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = glirc_is_channel(get_glirc(L), network, network_len,
                                                 target, target_len);
        lua_pushboolean(L, res);

        return 1;
}

/***
Resolve file path.

This provides access to the same path resolution logic used by the
client configuration file. Relative paths are resolved from the
directory containing the loaded configuration file. `~` is expanded to
the home directory.

@function resolve_path
@tparam string path Path
@treturn string Absolute file path
@usage
-- assuming configuration is at '/home/user/.config/glirc/config'
glirc.resolve_path('relative/path') --> 'home/user/.config/glirc/relative/path'
glirc.resolve_path('/absolute/path') --> '/abbsolute/path'
glirc.resolve_path('~/path') --> '/home/user/path'
*/
static int glirc_lua_resolve_path(lua_State *L)
{
        size_t path_len;
        const char *path;
        path = luaL_checklstring(L, 1, &path_len);
        luaL_checktype(L, 2, LUA_TNONE);

        char * res = glirc_resolve_path(get_glirc(L), path, path_len);
        lua_pushstring(L, res);
        glirc_free_string(res);

        return 1;
}

struct timer_state {
        lua_State *L;
        int fun_ref;  // callback function
        int self_ref; // timer_state allocation
};

static void free_timer_state(struct timer_state *st) {
        luaL_unref(st->L, LUA_REGISTRYINDEX, st->fun_ref);
        luaL_unref(st->L, LUA_REGISTRYINDEX, st->self_ref);
}

/* Create a new timer state closure around function on top of stack */
static struct timer_state *new_timer_state(lua_State *L) {
        struct timer_state * const ts = lua_newuserdata(L, sizeof(struct timer_state));
        ts->L = L;
        ts->self_ref = luaL_ref(L, LUA_REGISTRYINDEX);
        ts->fun_ref = luaL_ref(L, LUA_REGISTRYINDEX);
        return ts;
}

static void on_timer(void *dat, timer_id tid) {
        struct timer_state * const st = dat;
        lua_State * const L = st->L;

        // get callback function
        lua_rawgeti(L, LUA_REGISTRYINDEX, st->fun_ref); // STACK: closure

        // remove closure from timer closures table
        free_timer_state(st);

        if (lua_pcall(L, 0, 0, 0)) {
                // STACK: error
                size_t len;
                const char *msg = lua_tolstring(L, -1, &len);
                glirc_print(get_glirc(L), ERROR_MESSAGE, msg, len);
                lua_remove(L, -1);
        }
}

/***
Register a timer callback.

The given callback will be called after waiting at least
the given number of milliseconds.

@function set_timer
@tparam integer millis Milliseconds delay
@tparam func callback Callback function
@treturn integer Timer ID

@usage
glirc.set_timer(10000, function()
    glirc.print('10 seconds has passed')
end)
*/
static int glirc_lua_set_timer(lua_State *L)
{
        lua_Integer millis = luaL_checkinteger(L, 1);
        luaL_checkany(L, 2);
        luaL_checktype(L, 3, LUA_TNONE);

        // Wrap top (2) value into closure
        struct timer_state * const ts = new_timer_state(L);

        timer_id tid = glirc_set_timer(get_glirc(L), millis, on_timer, ts);

        lua_pushinteger(L, tid);

        return 1;
}

/***
Cancel an active timer by ID.

@function cancel_timer
@tparam integer Timer ID
@raise 'no such timer'
@usage
local tid = glirc.set_timer(1000, callback)
glirc.cancel_timer(tid)
*/
static int glirc_lua_cancel_timer(lua_State *L)
{
        lua_Integer tid = luaL_checkinteger(L, 1);
        luaL_checktype(L, 2, LUA_TNONE);

        struct timer_state * const st = glirc_cancel_timer(get_glirc(L), tid);

        if (st) {
                free_timer_state(st);
                return 0;
        } else {
                luaL_error(L, "no such timer");
        }

        return 0;
}

/***
Returns the list of window lines.
@function window_lines
@tparam string network Network name
@tparam string target Target name
@tparam boolean filtered Use the /grep filter
@treturn {string,...} Matched window lines
@usage glirc.window_lines('mynet', 'chatter', true)
*/
static int glirc_lua_window_lines(lua_State *L)
{
        size_t network_len, target_len;
        const char *network = luaL_checklstring(L, 1, &network_len);
        const char *target  = luaL_checklstring(L, 2, &target_len);
        int filtered = lua_toboolean(L, 3);
        luaL_checktype(L, 4, LUA_TNONE);

        char **res = glirc_window_lines(get_glirc(L), network, network_len,
                                                      target, target_len,
                                                      filtered);
        if (res == NULL) { luaL_error(L, "client failure"); }
        import_string_array(L, res);
        return 1;
}

struct system_state {
        lua_State *L;
        int self_ref; // reference to this allocation
        int callback_ref;
        atomic_int result;
        char command[];
};

void
thread_join_entrypoint(void *R) {
        struct system_state *st = R;
        struct lua_State *L = st->L;

        // Get function and arguments before deallocating st
        lua_rawgeti(L, LUA_REGISTRYINDEX, st->callback_ref);
        lua_pushinteger(L, st->result);

        luaL_unref(L, LUA_REGISTRYINDEX, st->callback_ref);
        luaL_unref(L, LUA_REGISTRYINDEX, st->self_ref); // deallocates R/st!

        if (lua_pcall(L, 1, 0, 0)) {
                size_t len;
                const char *msg = lua_tolstring(L, -1, &len);
                glirc_print(get_glirc(L), ERROR_MESSAGE, msg, len);
                lua_pop(L, 1);
        }
}

static void*
start_system(void *R) {
        struct system_state *st = R;
        st->result = system(st->command);
        return R;
}

/***
Run a system command in a separate thread given a continuation
for the result.
@function system
@tparam string command Shell command
@tparam function callback Callback to run on system return value
@usage glirc.window_lines('curl URL > tmpfile', print)
*/
int glirc_lua_system(struct lua_State *L) {

        // PROCESS ARGUMENTS
        size_t command_len;
        const char *command = luaL_checklstring(L, 1, &command_len);
        luaL_checkany(L, 2); // callback
        luaL_checktype(L, 3, LUA_TNONE);
        
        // BUILD THREAD STATE
        struct system_state *st = lua_newuserdata(L, sizeof *st + command_len + 1);
        st->self_ref = luaL_ref(L, LUA_REGISTRYINDEX);
        st->callback_ref = luaL_ref(L, LUA_REGISTRYINDEX);
        st->L = L;
        strcpy(st->command, command);

        // REQUEST NEW THREAD
        glirc_thread(get_glirc(L), start_system, st);
        return 0;
}

/***
Case-insensitive comparison of two identifiers using IRC case map.
Return -1 when first identifier is "less than" the second.
Return 0 when first identifier is "equal to" the second.
Return 1 when first identifier is "greater than" the second.
@function identifier_cmp
@tparam string identifier1 First identifier
@tparam string identifier2 Second identifier
@treturn integer Comparison result
@usage
glirc.identifier_cmp('somenick', 'SOMENICK') --> 0
glirc.identifier_cmp('surprise{|}~', 'surprise[\\]^') --> 0
glirc.identifier_cmp('apple', 'zebra') --> -1
glirc.identifier_cmp('zebra', 'apple') --> 1
*/
static int glirc_lua_identifier_cmp(lua_State *L)
{
        size_t str1_len, str2_len;
        const char *str1 = luaL_checklstring(L, 1, &str1_len);
        const char *str2 = luaL_checklstring(L, 2, &str2_len);
        luaL_checktype(L, 3, LUA_TNONE);

        int res = glirc_identifier_cmp(str1, str1_len, str2, str2_len);
        lua_pushinteger(L, res);

        return 1;
}

static void new_formatting_table(lua_State *L) {
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
  , { "inject_chat"       , glirc_lua_inject_chat        }
  , { "print"             , glirc_lua_print_string       }
  , { "error"             , glirc_lua_error              }
  , { "identifier_cmp"    , glirc_lua_identifier_cmp     }
  , { "list_networks"     , glirc_lua_list_networks      }
  , { "list_channels"     , glirc_lua_list_channels      }
  , { "list_channel_users", glirc_lua_list_channel_users }
  , { "my_nick"           , glirc_lua_my_nick            }
  , { "user_account"      , glirc_lua_user_account       }
  , { "user_channel_modes", glirc_lua_user_channel_modes }
  , { "channel_modes"     , glirc_lua_channel_modes      }
  , { "channel_masks"     , glirc_lua_channel_masks      }
  , { "mark_seen"         , glirc_lua_mark_seen          }
  , { "clear_window"      , glirc_lua_clear_window       }
  , { "current_focus"     , glirc_lua_current_focus      }
  , { "set_focus"         , glirc_lua_set_focus          }
  , { "is_logged_on"      , glirc_lua_is_logged_on       }
  , { "is_channel"        , glirc_lua_is_channel         }
  , { "resolve_path"      , glirc_lua_resolve_path       }
  , { "set_timer"         , glirc_lua_set_timer          }
  , { "cancel_timer"      , glirc_lua_cancel_timer       }
  , { "window_lines"      , glirc_lua_window_lines       }
  , { "system"            , glirc_lua_system             }
  , { NULL                , NULL                         }
  };

/* Helper function
 * Installs the 'glirc' library into the global environment
 * No stack effect
 */
void glirc_install_lib(lua_State *L)
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

        lua_pushcfunction(L, glirc_lua_print);
        lua_setglobal(L, "print");
}
