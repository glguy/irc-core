# Revision history for glirc2

## 2.19

* Smarter text box tracks "scroll" position independently from cursor
* Added `--full-version` flag

## 2.18

* Add digraph support under `M-k` and `/digraphs`
* Add ECDSA-NIST256P-CHALLENGE support for Freenode via Tor
* Load mask list on `/masks`
* Add `C-x` to change to next network window
* Allow `/clear NETWORK *` to clear all windows for the given network

## 2.17

* Add `reconnect-attempts` setting
* Add peristence for `/grep` and `/grepi`
* Add filter argument to `/windows`
* Better tab completion for `/channel` and `/focus`
* Isolate and number urls in view with `/url`
* Map `M-Left` and `M-Right` to backward word and forward word

## 2.16

* Add `/splits` to show multiple chat windows simultaneously

## 2.15

* Add `/mentions`
* Add macro argument declarations
* Add indication when a command is still a prefix or not of a valid command
* Support quoted strings arguments to /exec
* Add F4 to toggle visibility of metadata lines
* tls-insecure setting was incorrectly behaving like normal insecure
* Add `C-t` to swap characters
* Add `ESC` to return to messages window

## 2.14

* Add `/help`
* Add `/palette`
* Add F3 to toggle activity detail bar

## 2.13

* Add disconnect expansion, support expansions in connect-cmds
* Add default expansion syntax `${var|default}`
* Add support for multiple nicknames to try on connect
* Add `ignores` section to configuration
* Add `url-opener` section to configuration and `/url` command

## 2.12

* Remove `tls-insecure` configuration option in favor of `tls: yes-insecure`
* Implement fancy command placeholder rendering and argument parsing
* Improved reconnect logic
* Improved connection error messages

## 2.11

* Add `M-S` to jump to previously focused window
* Add `extra-highlights` section
* Tab complete servernames in `/connect`
* Add `/windows` command for listing active windows
* Add `glirc_clear_window` C API procedure
* Allow `process_message` callback to drop messages
* Add optional network and channel arguments to `/clear` (intended to assist macros)
* Automatically reconnect on ping timeout
* Many commands will report message to client window on error

## 2.10

* Fixes for multiline editing
* Multiple, sequential kills all fill the same yank buffer

## 2.9

* Dynamically loadable extensions
* Implement Lua scripting extension
* Enable support for batch messages
* Grow metadata lines to the right

## 2.8

* Support `vty-5.8`
* Implement inital support for macros
* Support `znc.in/self-message`

## 2.7

* Switch to regex-tdfa (easier to install on macOS than text-icu)
* Tab-complete starts with most recent nick
* Add `/reload`
* Add custom palette entry for self highlights
* Add ability to set background colors and styles in palette

## 2.6

* connect-cmds now use actual client commands instead of raw IRC messages. For example `msg user my message` or `join #mychannel`
* Multiple lines can be held in the textbox at once. Pasting mutiple lines insert those lines into the textbox rather than sending them immediately.
* Added `M-d` and `M-Enter` key bindings
* Added `name` field to server configuration
* Extract irc-core library again
* Configurable self color

## 2.5

* Add facilities for hooks that can alter the irc message stream.
* Implement a hook that handles the znc buffextras plugin.
* Implement configurable nick color highlight palette.
* Resolve relative paths starting at the home directory.
* Significantly configurable UI colors

## 2.4

* Support XDG configuration directory, e.g. `~/.config/glirc/config`
* Add more window names. Shift selects second set of names.
* Add `/channel` and `/say`
* Improve `/focus` tab completion

## 2.3

* Add commands `/znc`
* Add initial support for ZNC's playback module and `/znc-playback` command
* Don't consider message seen when in masklist, userlist, or channelinfo windows
* Add terminal bell on command error

## 2.2

* Add commands `/ison`, `/userhost`, `/away`, `/notice`, `/ctcp`, `/links`, `/time`, `/stats`
* Added context-sensitive completion to `/mode`
* Render CTCP messages
* Memory performance improvements
* Improved logic on nick changes
* Support for fractional flood settings
* Fixed VTY formatting bug
* Add counts to the mask and user lists

## 2.1

* Add red highlighting for own nick
* Synchronize reply codes with Freenode
* Add textual interpretation of reply codes
* Add SASL support
* Add `/channelinfo` command

## 2.0

* First version of glirc rewrite
