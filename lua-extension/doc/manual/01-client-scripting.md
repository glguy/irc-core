## Installing the Lua extension

First you'll need Lua 5.3. You can install this from your package manager.

```bash
$ brew install lua
```

Next you'll need to build the extension and install it.

```bash
$ cd glirc/lua-extension
$ make
$ cp glirc-lua.dylib ~/.config/glirc/
```

Then you can configure your client to use the extension. The Lua extension uses the first argument as the path to the Lua script and the remaining arguments are available in the `arg` table. If no arguments are provided, the extension defaults to a script named `glirc.lua` in the same directory as the extension.

```
extensions:
  * path: "glirc-lua.dylib"
    args: [ "my_script.lua" ]
```

You'll need to write your script. You can build the documentation locally with `ldoc -a glirc-lua.c` or see the online version at https://glguy.net/glirc-lua-doc/ .

Your script is expected to return a table of callbacks as described in the documentation above. Hooks back into the client are available in the global `glirc` table.

### Example minimal script

```lua
local M = {}
function M:process_message(msg)
    glirc.print('Message received: ' .. msg.command .. ' ' .. table.concat(msg.params, ' '))
end
return M
```
