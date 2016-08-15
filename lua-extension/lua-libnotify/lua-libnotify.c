#include <libnotify/notify.h>
#include <lua.h>
#include <lauxlib.h>

static int Ninit(lua_State *L) {

        const char *name = luaL_checkstring(L, 1);

        /* This is necessary to get things going again after dlclose */
        g_type_init();

        gboolean success = notify_init(name);

        if (success == FALSE) {
                luaL_error(L, "notify_init failed");
        }
        return 0;

}

static int Nuninit(lua_State *L) {

        notify_uninit();
        return 0;

}

static int Nnotify(lua_State *L) {

        int n = lua_gettop(L);
        const char *summary = luaL_checkstring(L, 1);
        const char *body    = n >= 2 ? luaL_checkstring(L, 2) : NULL;
        const char *icon    = n >= 3 ? luaL_checkstring(L, 3) : NULL;

        NotifyNotification * N = notify_notification_new(summary, body, icon);

        if (N == NULL) {
                luaL_error(L, "notify_notification_new failed");
        }

        gboolean success = notify_notification_show(N, NULL);

        g_object_unref(G_OBJECT(N));

        if (success == FALSE) {
                luaL_error(L, "notify_notification_show failed");
        }

        return 0;

}

luaL_Reg funcs[] =
  { { "init"  , Ninit }
  , { "uninit", Nuninit }
  , { "notify", Nnotify }
  };

int luaopen_libnotify(lua_State *L) {
        luaL_newlib(L, funcs);
        return 1;
}
