## Installing the Lua extension

First you'll need Lua 5.3. You can install this from your package manager.

```bash
$ brew install lua
```

Next you'll need to build the extension and install it. The extension is
built using the [Meson Build System](http://mesonbuild.com) and
[Ninja](https://ninja-build.org) build tool.

```bash
$ brew install meson ninja # install build system
$ cd lua-extension         # change to glirc subdirectory
$ meson builddir           # generate build directory
$ cp builddir              # switch to build directory
$ ninja                    # build the module
$ cp glirc-lua.bundle ~/.config/glirc/ # install the module
```

Then you can configure your client to use the extension. The Lua
extension uses the first argument as the path to the Lua script and the
remaining arguments are available in the `arg` table. If no arguments
are provided, the extension defaults to a script named `glirc.lua` in
the same directory as the extension. The script path uses the same
resolution as other files in the client: paths are relative to the
configuration file and `~` is expanded to the home directory.

```
extensions:
  * path: "glirc-lua.dylib"
    args: [ "my_script.lua" ]
```

You can build the documentation locally with or see the online version
at <https://glguy.net/glirc-lua-doc/>.

```
$ luarocks install ldoc
$ cd lua-extension/doc
$ ldoc -a glirc-lua.c
```

Your script is expected to return a table of callbacks as described in
`extension`. Client interaction is available through the `glirc`
library. This library is provided when your script is loaded.

### Example minimal script

```lua
local M = {}
function M:process_message(msg)
    glirc.print('Message received: ' .. msg.command .. ' ' .. table.concat(msg.params, ' '))
end
return M
```
