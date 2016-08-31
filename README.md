My IRC client
=============

[![Build Status](https://secure.travis-ci.org/glguy/irc-core.svg)](http://travis-ci.org/glguy/irc-core)

![](https://raw.githubusercontent.com/wiki/glguy/irc-core/images/screenshot.png)

Building
========

Note that glirc currently requires GHC 8.0.1

glirc uses recent versions of packages, make sure you package databases are
up-to-date:

```
$ cabal update # if you're using cabal
$ stack update # if you're using stack
```

To install the latest version from Hackage using cabal-install:

```
$ cabal install glirc
```

Building with cabal-install from source checkout

```
$ cabal install --dep
$ cabal build
```

Building with stack using ghc-8 resolver (nightly resolvers can work using --solver)

```
$ stack init --resolver=ghc-8
$ stack build
```

Client Features
===============

* Subsequent joins and parts fold into one line and do not scroll chat messages off the screen
* Ignore support that folds ignored messages into the joins and parts. Toggle it off to see previously hidden messages
* Detailed view to see all the messages in a channel in full detail with hostmask and timestamp (F2)
* Nick tab completion
* New message notification
* View ban, quiet, invex, and exception lists
* WYSIWYG mIRC formatting input
* Chanserv integration
* Each user's nick is assigned a consistent color, when a user's nick is rendered in a chat message it uses that same color.
* Support for /STATUSMSG/ messages (messages only voice or op users can see)
* Run commands upon connection
* Ban lists don't obstruct chat messages
* Ban list and user list are searchable
* CERTFP and SASL authentication

TLS
===

`glirc` has TLS support via the Haskell `tls` package. Note that Freenode (and other networks) will allow you to authenticate to NickServ via a client certificate.

I use the `x509-store` for decoding certificates and private key files. This library seems to support PEM formatted files and does not seem to support encrypted private key files. If the key and certificate are both contained in the certificate file the private key command line argument is unnecessary.

Startup
=======

```
glirc [FLAGS] INITIAL_NETWORKS...
  -c PATH  --config=PATH  Configuration file path
  -h       --help         Show help
  -v       --version      Show version
```

Environment variables
```
USER=<default nickname and username>
IRCPASSWORD=<your irc password>
```

Configuration file
=================

A configuration file can currently be used to provide some default values instead of
using command line arguments. If any value is missing the default will be used.

The default configuration file path is `~/.config/glirc/config`

Relative paths are relative to the home directory.

Learn more about this file format at [config-value](http://hackage.haskell.org/package/config-value)

```
-- Defaults used when not specified on command line
defaults:
  port:            6667
  nick:            "yournick"
  username:        "yourusername"
  realname:        "Your real name"
  password:        "IRC server password"
  tls:             yes -- or: yes-insecure or no
  tls-client-cert: "/path/to/cert.pem"
  tls-client-key:  "/path/to/cert.key"

-- Override the defaults when connecting to specific servers
servers:
  * hostname:      "chat.freenode.net"
    sasl-username: "someuser"
    sasl-password: "somepass"
    socks-host:    "socks5.example.com"
    socks-port:    8080 -- defaults to 1080

  * name: "example"
    hostname:      "example.com"
    port:          7000
    connect-cmds:
      * "join #favoritechannel,#otherchannel"
      * "msg mybot another command"

    -- Specify additional certificates beyond the system CAs
    -- relative to home directory
    server-certificates:
      * "extra/certificate.pem"

macros:
  * name: "wipe"
    commands:
      * "clear"
      * "znc *status clearbuffer $channel"

  -- Example use of macro in combination with an extension
  * name: "extra"
    commands:
      * "extension Lua some-parameter $network $channel"

extra-highlights: ["glirc", "lens"]

palette:
  time:
    fg: [10,10,10] -- RGB values for color for timestamps
    bg: blue
  nick-colors:
    [ cyan, magenta, green, yellow, blue
    , bright-cyan, bright-magenta, bright-green, bright-blue
    , 218,  88,  89, 124, 160, 205, 212, 224 -- reds
    ,  94, 130, 166, 172, 208, 214, 216, 180 -- oranges
    ,  58, 226, 229, 184, 187, 100, 142, 220 -- yellows
    ,  22,  34,  40,  82,  70,  64,  48,  85 -- greens
    ,  25,  27,  33,  39,  51,  80,  81,  75 -- blues
    ,  69,  61,  56,  54, 129,  93,  99, 147 -- purples
    ]
```

Configuration sections:
--------

* `defaults` - These settings are used for all connections
* `servers` - These settings are used to override defaults when the hostname matches
* `palette` - Client color overrides
* `window-names` - text - Names of windows (typically overridden on non QWERTY layouts)
* `nick-padding` - nonnegative integer - Nicks are padded until they have the specified length
* `extra-highlights` - list of text - Extra words/nicks to highlight
* `extensions` - list of text - Filenames of extension to load

Settings
--------

* `name` - text - name of server entry, defaults to `hostname`
* `hostname` - text - hostname used to connect and to specify the server
* `port` - number - port number, defaults to 6667 without TLS and 6697 with TLS
* `nick` - text - nickname
* `username` - text - username
* `realname` - text - realname / GECOS
* `password` - text - server password
* `sasl-username` - text - SASL username
* `sasl-password` - text - SASL password
* `tls` - yes/yes-insecure/no - use TLS to connect (insecure mode disables certificate checks)
* `tls-client-cert` - text - path to TLS client certificate
* `tls-client-key` - text - path to TLS client key
* `connect-cmds` - list of text - client commands to send upon connection
* `socks-host` - text - hostname of SOCKS proxy to connect through
* `socks-port` - number - port number of SOCKS proxy to connect through
* `server-certificates` - list of text - list of CA certificates to use when validating certificates
* `chanserv-channels` - list of text - list of channels with chanserv op permission
* `flood-penalty` - number - cost in seconds per message
* `flood-threshold` - number - threshold of seconds for burst
* `message-hooks` - list of text - names of hooks to enable

Palette
-------

* `nick-colors` - List of attr - Use for nick highlights
* `self` - attr - attr of our own nickname(s) outside of mentions
* `self-highlight` - attr - attr of our own nickname(s) in mentions (defaults to `self`)
* `time` - attr - attr for timestamp
* `meta` - attr - attr for metadata
* `sigil` - attr - attr for sigils
* `label` - attr - attr for information labels
* `latency` - attr - attr for latency time
* `error` - attr - attr for error messages
* `textbox` - attr - attr for textbox edges
* `window-name` - attr - attr for current window name
* `activity` - attr - attr for activity notification
* `mention` - attr - attr for mention notification
* `command` - attr - attr for recognized command
* `command-ready` - attr - attr for command with successful parse
* `command-required` - attr - attr for required command argument placeholder
* `command-optional` - attr - attr for optional command argument placeholder
* `command-remaining` - attr - attr for remaining command text placeholder

Text Attributes
---------------

Text attributes can be specified either as a single foreground color or section of attributes.

* `<number>` - Maps to a terminal color
* `<name>` - Direct selection of standard 16 terminal colors
* `[red-number, blue-number, green-number]` - List of 3 numbers in range 0-255 map to an approximation of the RGB color.

Attributes

* `fg` - foreground color
* `bg` - background color
* `style` - single style or list of styles

Styles

* `blink`
* `bold`
* `dim`
* `standout`
* `reverse-video`
* `underline`

Commands
========

Client commands

* `/exit` - Terminate the client
* `/quit` - Gracefully terminate connection to the current server
* `/connect <name>` - Connect to the given server
* `/disconnect` - Forcefully terminate connection to the current server
* `/reconnect` - Reconnect to the current server
* `/reload` - Reload the previous configuration file (not retroactive!)
* `/reload <path>` - Load a new configuration file
* `/windows` - List all open windows
* `/extension <extension name> <params>` - Send the given params to the named extension
* `/exec [-n network] [-c channel] <command> <arguments>` - Execute a command, If no network or channel are provided send output to client window, if network and channel are provided send output as messages, if network is provided send output as raw IRC messages.

Connection commands

* `/nick <nick>` - Change nickname
* `/away <message>` - Set away status

Window management

* `/focus <server>` - Change focus to server window
* `/focus <server> <channel>` - Change focus to channel window
* `/clear [network] [channel]` - Clear contents of current or specified window
* `/ignore <nick>` - Toggle ignore of a user
* `/channel <channel>` - Change focus to channel on current network (alias: `/c`)

Channel membership

* `/join <channel>` - Join a channel (alias: `/j`)
* `/part` - Part from current channel

Chat commands

* `/msg <target> <msg>` - Send a message on the current server to target
* `/notice <target> <msg>` - Send a notice message on the current server to target
* `/ctcp <target> <command> <args>` - Send a ctcp command on the current server to target
* `/me <msg>` - Send action message to channel
* `/say <msg>` - Send normal message to channel

Channel management

* `/mode <mode> <params>` - Change modes on the current channel (advanced tab completion)
* `/kick <nick>` - Kick a user
* `/kickban <nick>` - Kick and ban a user
* `/remove` - Gracefully kick a user
* `/topic <topic>` - Change the topic (tab completion for topic)
* `/invite <nick>` - Invite a user to the current channel

Queries

* `/who <query>` - Perform WHO query (use detailed view to see output)
* `/whois <nick>` - Perform WHOIS query
* `/whowas <nick>` - Perform WHOWAS query
* `/ison <nick>` - Perform ISON query
* `/userhost <nick>` - Perform USERHOST query
* `/links <server>` - Perform LINKS query
* `/time` - Perform TIME query
* `/stats <query>` - Perform STATS query

Channel information

* `/users` - Show channel user list
* `/masks <mode>` - Show channel bans(b), quiets(q), exempts(e), or invex(I)
* `/channelinfo` - Show channel topic, creation, url

Window filters

* `/grep` - Filter chat messages using a regular expression
* `/grepi` - Filter chat messages using a case-insensitive regular expression on the message

ZNC-specific

* `/znc <module> <parameters>` - send command to ZNC module without echoing to all clients
* `/znc-playback` - ZNC playback module - play everything
* `/znc-playback <time>` - ZNC playback module - play everything start at the given time today
* `/znc-playback <date> <time>` - ZNC playback module - play everything start at the given time

Low-level

* `/quote <raw command>` - Send a raw IRC command to the server

Keyboard Shortcuts
==================

* `^N` next channel
* `^P` previous channel
* `M-#` jump to window - `1234567890qwertyuiop!@#$%^&*()QWERTYUIOP`
* `M-A` jump to activity
* `M-S` jump to previous window
* `^A` beginning of line
* `^E` end of line
* `^K` delete to end
* `^U` delete to beginning
* `^D` delete at cursor
* `^W` delete word backwards
* `^Y` paste from yank buffer
* `M-F` forward word
* `M-B` backward word
* `M-BACKSPACE` delete word backwards
* `M-D` delete word forwards
* `TAB` nickname completion
* `F2` toggle detailed view
* `Page Up` scroll up
* `Page Down` scroll down
* `^B` bold
* `^C` color
* `^V` reverse video
* `^_` underline
* `^]` italic
* `^O` reset formatting
* `M-Enter` insert newline

Macros
======

The `macros` configuration section allows you to define
sequences of commands. These commands can contain expansions.

Configuration
-------------

* `name` - text - name of macro
* `commands` - list of text - commands to send after expansion

Macro Expansions
----------------

Variable names and integer indexes can be used when defining commands.
Variables are specified with a leading `$`. For disambiguation a variable
name can be surrounded by `{}`. `$channel` and `${channel}` are
equivalent.

* `channel` - current channel
* `network` - current network name
* `nick` - current nickname

The arguments to a command will be mapped to integer indexes. The command
itself is at index zero.

* `0` - command
* `1` - first argument
* `2` - second argument (etc.)

Hooks
=====

buffextras
----------

Enable this hook when using ZNC and the `buffextra` module in order to reinterpret
this module's messages natively in the client.
