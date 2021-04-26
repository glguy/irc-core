#include "glirc-marshal.h"

static char token_key;

struct glirc *get_glirc(lua_State *L)
{
        lua_rawgetp(L, LUA_REGISTRYINDEX, &token_key);
        struct glirc *token = lua_touserdata(L, -1);
        lua_pop(L, 1);
        return token;
}

void set_glirc(lua_State *L, struct glirc *G)
{
        lua_pushlightuserdata(L, G);
        lua_rawsetp(L, LUA_REGISTRYINDEX, &token_key);
}

/* Helper
 * Pushes the string represented by the argument to the top of the stack
 *
 * [-0, +1, -]
 */
int get_glirc_string(lua_State *L, int i, struct glirc_string *s)
{
        s->str = lua_tolstring(L, i, &s->len);
        return !s->str;
}

/* Push the string contained in s on the top of the stack
 *
 * [-0, +1, m]
 * */

void push_glirc_string(lua_State *L, const struct glirc_string *s)
{
        lua_pushlstring(L, s->str, s->len);
}

/* Helper function
 * Returns: Array of strings
 * Import the given array of strings, free the strings and the list
 *
 * [-0, +1, m]
 */
void import_string_array(lua_State *L, char **list)
{
        int count = 0;
        while (list[count] != NULL) {
                count++;
        }

        lua_createtable(L, count, 0);

        for (int i = 0; list[i] != NULL; i++) {
                lua_pushstring(L, list[i]);
                lua_rawseti(L, -2, i+1);
        }

        glirc_free_strings(list);
}

/* Push a table onto the top of the stack containing all of the fields
 * of the command struct
 *
 * [-0, +1, m]
 * */
void push_glirc_command(lua_State *L, const struct glirc_command *cmd)
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
void push_glirc_message(lua_State *L, const struct glirc_message *msg)
{
        lua_createtable(L, 0, 5);

        push_glirc_string(L, &msg->network);
        lua_setfield(L,-2,"network");

        lua_createtable(L, 0, 3);
        push_glirc_string(L, &msg->prefix_nick);
        lua_setfield(L, -2, "nick");
        push_glirc_string(L, &msg->prefix_user);
        lua_setfield(L, -2, "user");
        push_glirc_string(L, &msg->prefix_host);
        lua_setfield(L, -2, "host");
        lua_setfield(L, -2, "prefix");

        push_glirc_string(L, &msg->command);
        lua_setfield(L, -2, "command");

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

/* Push a table onto the top of the stack containing all of the fields
 * of the chat struct
 *
 * [-0, +1, m]
 * */
void push_glirc_chat(lua_State *L, const struct glirc_chat *chat)
{
        lua_createtable(L, 0, 3);

        push_glirc_string(L, &chat->network);
        lua_setfield(L,-2,"network");

        push_glirc_string(L, &chat->target);
        lua_setfield(L,-2,"target");

        push_glirc_string(L, &chat->message);
        lua_setfield(L,-2,"message");
}
