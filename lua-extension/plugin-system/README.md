# glirc Lua plugin-based extension

This provides a basic plugin-architecture for organizing your glirc Lua
scripting.

## Getting started

(Note: On Linux the library will be `glirc-lua.so` instead of `glirc-lua.bundle`)

```
$ luarocks install penlight # install dependencies
$ cd /path/to/lua-extension
$ meson build
$ cd build
$ ninja
$ cp glirc-lua.bundle ~/.config/glirc/glirc-lua.bundle
$ cd ..
$ cp -r plugin-system ~/.config/glirc/lua
```

Update your configuration file:

```
extensions:
  * path: "glirc-lua.bundle"
    args: [ "lua/extension.lua", "Eval" ]
```

Any additional plugins should be listed after `"Eval"`.

## Plugin API

Plugins install IRC message handlers in the `messages` table.
Plugins install client command handlers in the `commands` table.
See `Eval.lua` for an example command handler.
