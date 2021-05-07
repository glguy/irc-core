#include "glirc-warn.h"
#include "glirc-api.h"
#include "glirc-marshal.h"
#include <lauxlib.h>
#include <string.h>
#include <stdlib.h>

struct buffer {
    char *ptr;
    size_t len;
    size_t cursor;
};

static
void add_to_buffer(struct buffer *buffer, const char *msg)
{
    size_t msg_len = strlen(msg);

    if (buffer->len - buffer->cursor <= msg_len) {
        size_t new_len = buffer->len < 512 ? 512 : 2 * buffer->len;
        while (new_len - buffer->cursor <= msg_len) {
            new_len *= 2;
        }
        char *res = realloc(buffer->ptr, new_len);
        if (res) {
            buffer->ptr = res;
            buffer->len = new_len;
        } else {
            return;
        }
    }

    strcpy(buffer->ptr + buffer->cursor, msg);
    buffer->cursor += msg_len;
}

static void init_buffer(struct buffer *buffer)
{
    buffer->ptr = NULL;
    buffer->len = 0;
    buffer->cursor = 0;
}

static void free_buffer(struct buffer *buffer)
{
    free(buffer->ptr);
    init_buffer(buffer);
}

struct warning_st {
        struct glirc *G;
        struct buffer buffer;
        int enabled;
};

static void warning(void *ud, const char *msg, int tocont)
{
    struct warning_st *st = ud;

    // Beginning of warning sequence
    if (st->buffer.cursor == 0) {
        // control messages
        if (*msg == '@') {
            if (strcmp(msg, "@off") == 0) {
                st->enabled = 0;
            } else if (strcmp(msg, "@on") == 0) {
                st->enabled = 1;
            }
            return;
        }

        if (!st->enabled) {
            return;
        }

        add_to_buffer(&st->buffer, "\0037Lua warning\017: ");
    }

    add_to_buffer(&st->buffer, msg);

    if (!tocont) {
        glirc_print(st->G, ERROR_MESSAGE, st->buffer.ptr, st->buffer.cursor);
        free_buffer(&st->buffer);
    }
}

static int gc_warning_st(lua_State *L) {
    struct warning_st *st = lua_touserdata(L, 1);
    free_buffer(&st->buffer);
    return 0;
}

static luaL_Reg warning_st_mt[] = {
    {"__gc", gc_warning_st},
    {NULL, NULL}
};

void setup_warnings(lua_State *L)
{
    struct warning_st *st = lua_newuserdatauv(L, sizeof *st, 0);
    st->G = get_glirc(L);
    st->enabled = 1;
    init_buffer(&st->buffer);

    luaL_newlib(L, warning_st_mt);
    lua_setmetatable(L, -2);
    lua_rawsetp(L, LUA_REGISTRYINDEX, st);

    lua_setwarnf(L, warning, st);
}
