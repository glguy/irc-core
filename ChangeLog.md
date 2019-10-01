# Revision history for glirc2

## 2.33.1

* Support for GHC 8.8.1
* Add `/trace` command

## 2.33

* Fixed cursor position with wide characters in input box
* Added ability to force request capabilities.
* Generalized `FreRelay` hook
* Add `show-accounts` setting to make account status visible
* Query channel modes on join

## 2.32

* Fix SASL EXTERNAL
* Better /url matching
* Support for config-schema improvements

## 2.31

* Added TLS fingerprint pinning with `tls-cert-fingerprint` and `tls-pubkey-fingerprint`
* Addded `/cert` command
* Improved C API `void*` passing to always pass around a single pointer.
* Better rendering for `/whois` and `/whowas`
* Better support for ircop commands, responses, and modes
* Channel, user, and snomask modes are can be styled
* Bugfix in textbox rendering for formatted multi-line inputs.
* Improved `/palette` rendering
* Switch window focus when *forwarded*
* Add support for receiving files with DCC (thanks Ruben Astudillo)

## 2.30

* Implement support for chghost <https://ircv3.net/specs/extensions/chghost-3.2.html>
* Implement support for userhost-in-names <https://ircv3.net/specs/extensions/userhost-in-names-3.2.html>
* Implement support for SASL EXTERNAL authentication
* Implement support for CAP 302 <https://ircv3.net/specs/core/capability-negotiation.html>
* Implement support for sts <https://ircv3.net/specs/extensions/sts.html>
* Add ACCOUNT messages to the metadata lines
* Add logic for server defined user modes
* Ignore leading whitespace when interpreting commands
* Handle more formatting color codes

## 2.29
* Add support for timers to the extension API
* Add `glirc_set_focus` API call

## 2.28
* Add formatting reference to `/palette`
* Make RTLD flags configurable when loading an extension
* Update client's internal hostname on `RPL_HOSTHIDDEN`
* Support IRCv3.2 `server-time` extension over ZNC-specific one
* Lots of work on the Lua extension code and documentation
* Added `glirc_user_account` and `glirc_user_channel_modes`
  extension API entrypoints.
* Added support for `account-notify` and `extended-join`
* Added configurable "arguments" to the C extensions

## 2.27
* Requires GHC 8.4
* Show channel topic in channel on join
* Fix socket leak when failing to load TLS certificates
* Add delay when indicating sent ping to reduce noise
* Add support for italics in formatting

## 2.26
* Updates for GHC 8.4.1
* Added `/toggle-show-ping` and `show-ping` configuration setting
  to toggle visibility of the ping round-trip times.

## 2.25
* `/ignore` can list ignores and supports full wildcard masks
* Updated C extension API
* Improved OTR extension to avoid interacting with ZNC replays
* Added `protocol-family` configuration option. Set to `inet` or
  `inet6` to force a particular IP protocol.

## 2.24

* `/query` now takes a message parameter and tab-completes
  like `/msg`.

## 2.23

* CONFIGURATION FILE CHANGE: Relative paths are now resolved
  from the directory of the configuration file. The "~" alias
  for the home directory works for resolving paths relative
  to $HOME. Absolute paths are unmodified.

* Support `network:#channel` in `/query`

* Add an optional OTR extension. See the `otr-extension/` directory
  and the wiki for more information.

* Extended the C API with `glirc_inject_chat` and `chat_entrypoint`

* Smarter WYSIWYG highlighting in text edit box. Nicknames are
  highlighted and `/mode` command parameters get correct placeholders.

* Added more information to `/palette`

* Added "human readable" labels to `/rtsstats`

## 2.22

* Added dynamic indentation of wrapped lines. Lines now
  wrap to where the message portion of the line started.
  This removes `indent-wrapped-lines` configuration setting
* Added pervasive word-boundary oriented line wrapping
  (beyond what was previously restricted to chat messages)
* Made nick prefix padding configurable to be on left or right.
  See `nick-padding` setting in `glirc2 --config-format`.
  In addition this setting is now reconfigurable at runtime
  via `/reload`
* Added `/oper` command for network operator authentication.
* Memory savings by packing message timestamp information, and
  using the "detailed" image for regular expression matching.
* Added `/rtsstats` command for inspecting the GHC RTS statistics
* Added many minor IRC query commands: `admin`, `info`, `map`, `rules`
  `motd`, `version`, `lusers`, `kill`, `knock`, `list`.

## 2.21.1

* Support for latest `config-schema`
* Adapt extension API to help support stub Rust extension

## 2.21

* Make metadata toggle `F4` a window-level setting instead of client level
* Add configuration option to hide metadata by default `hide-metadata`
* Make keymap configurable under `key-bindings`, add `/keymap` command
* Add transient error message view, press `ESC` to clear
* Implement two-column split window mode: `/toggle-layout` and `F5`
* Implement word-boundary based line wrapping

## 2.20.6

* Switch to new `config-schema` package for configuration file loading.
* Add `--config-format` flag to executable to show configuration file format.

## 2.20.5

* Add line indicating message since previous time window was focused.
  The palette for this is configurable as `line-marker`

## 2.20.4

* Add `/query` alias for `/channel`
* Add `/names` alias for `/users`
* Update to build on GHC 8.2.1-rc1 with Cabal-2.0.0.0

## 2.20.3

* Nicer `/help` system, commands are grouped
* Added `/splits+` and `/splits-` for incremental updates to splits
* Jump-to-activity returns to original window after activity is visited
* Extended activity view makes use of empty space above text input
* Parse the timestamp and duration from `/whois` response

## 2.20.2.1

* Support `vty-5.15`

## 2.20.2

* Remove `memory` dependency
* Add `indent-wrapped-lines` setting

## 2.20.1.1
* Remove macro dependency on happy and alex being installed for version information

## 2.20.1
* Support `vty-5.11.1`

## 2.20

* Move from `tls` to `HsOpenSSL` support via the new `hookup` package

## 2.19

* Smarter text box tracks "scroll" position independently from cursor
* Added `--full-version` flag
* Remove `regex-tdfa-text` dependency
* Added `bell-on-mention` client setting
* Added `ExportCApi` cabal flag to help with loading the client in GHCi

## 2.18

* Add digraph support under `M-k` and `/digraphs`
* Add ECDSA-NIST256P-CHALLENGE support for Freenode via Tor
* Load mask list on `/masks`
* Add `C-x` to change to next network window
* Allow `/clear NETWORK *` to clear all windows for the given network

## 2.17

* Add `reconnect-attempts` setting
* Add persistence for `/grep` and `/grepi`
* Add filter argument to `/windows`
* Better tab completion for `/channel` and `/focus`
* Isolate and number URLs in view with `/url`
* Map `M-Left` and `M-Right` to backward word and forward word

## 2.16

* Add `/splits` to show multiple chat windows simultaneously

## 2.15

* Add `/mentions`
* Add macro argument declarations
* Add indication when a command is still a prefix or not of a valid command
* Support quoted strings arguments to /exec
* Add `F4` to toggle visibility of metadata lines
* `tls-insecure` setting was incorrectly behaving like normal insecure
* Add `C-t` to swap characters
* Add `ESC` to return to messages window

## 2.14

* Add `/help`
* Add `/palette`
* Add `F3` to toggle activity detail bar

## 2.13

* Add disconnect expansion, support expansions in `connect-cmds`
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
* Tab complete server names in `/connect`
* Add `/windows` command for listing active windows
* Add `glirc_clear_window` C API procedure
* Allow `process_message` callback to drop messages
* Add optional network and channel arguments to `/clear` (intended to assist macros)
* Automatically reconnect on ping timeout
* Many commands will report message to client window on error

## 2.10

* Fixes for multi-line editing
* Multiple, sequential kills all fill the same yank buffer

## 2.9

* Dynamically loadable extensions
* Implement Lua scripting extension
* Enable support for batch messages
* Grow metadata lines to the right

## 2.8

* Support `vty-5.8`
* Implement initial support for macros
* Support `znc.in/self-message`

## 2.7

* Switch to `regex-tdfa` (easier to install on MacOS than `text-icu`)
* Tab-complete starts with most recent nick
* Add `/reload`
* Add custom palette entry for self highlights
* Add ability to set background colors and styles in palette

## 2.6

* `connect-cmds` now use actual client commands instead of raw IRC messages. For example `msg user my message` or `join #mychannel`
* Multiple lines can be held in the textbox at once. Pasting multiple lines insert those lines into the textbox rather than sending them immediately.
* Added `M-d` and `M-Enter` key bindings
* Added `name` field to server configuration
* Extract `irc-core` library again
* Configurable self color

## 2.5

* Add facilities for hooks that can alter the IRC message stream.
* Implement a hook that handles the ZNC `buffextras` plugin.
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
* Don't consider message seen when in `masklist`, `userlist`, or `channelinfo` windows
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
