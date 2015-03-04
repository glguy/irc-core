My IRC client
=============

This client has fewer features than the one you're using now. You probably don't want to use it.

![](https://raw.githubusercontent.com/wiki/glguy/irc-core/images/screenshot.png)

Library
=======

This package is split into a generic IRC modeling library and a VTY-base text client using that library.

Startup
=======

```
glirc <options> SERVER
  -p PORT  --port=PORT  IRC Server Port
  -n NICK  --nick=NICK  Nickname
  -u USER  --user=USER  Username
  -r REAL  --real=REAL  Real Name
  -h       --help       Show help
```

Commands
========

* `/server` - switch to the server message window
* `/quote <nick>` - switch to a user message window
* `/channel <channel>` - switch to a user message window
* `/msg <nick> <message>` - send a private message
* `/notice <nick> <message>` - send a notice message
* `/me <message>` - send an action to the current channel
* `/join <channel>` - join a new channel (optional key argument)
* `/part <message>` - part the current channel with the given message
* `/quote <raw client command>` - send a client command verbatim
* `/channelinfo` - Show information for the current channel
* `/bans` - Show known bans for current channel. Note: Request bans list with `/quote mode <channel> +b`
* `/masks <mode>` - Show the bans (b), quiets (q), invex (I), or ban exemptions (e) for a channel. The list must be requested as above.
* `/mode <mode> <arguments>` - Set modes on the current channel
* `/umode <mode>` - Set modes on yourself
* `/kick <nick> <msg>` - Kick a user from the current channel
* `/remove <nick> <msg>` - Force a user to part from the current channel
* `/hs <haskell source code>` - Send syntax highlighted source code as a message to the current channel
* `/whois <nick>` - Query the server for information about a user
* `/topic <topic>` - Change the topic for the current channel
* `/ignore <nick>` - Toggle ignoring a user by nickname.
* `/clear` - Clear all messages for the current channel

Keyboard Shortcuts
==================

* `ESC` quit
* `^N` next channel
* `^P` previous channel
* `^A` beginning of line
* `^E` end of line
* `^K` delete to end
* `^U` delete to beginning
* `^D` delete at cursor
* `^W` delete word
* `M-F` forward word
* `M-B` backward word
* `TAB` nickname completion
* `F2` toggle detailed view
* `Page Up` scroll up
* `Page Down` scroll down
