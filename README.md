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
           --sasl-user=USER SASL Username
  -h       --help       Show help

Environment variables
IRCPASSWORD=<your irc password>
SASLPASSWORD=<your sasl password>
```

Commands
========

* `/bans` - Show known bans for current channel. Note: Request bans list with `/quote mode <channel> +b`
* `/channel <channel>` - switch to a user message window
* `/channelinfo` - Show information for the current channel
* `/clear` - Clear all messages for the current channel
* `/help <topic>` - Request help from the server
* `/hs <haskell source code>` - Send syntax highlighted source code as a message to the current channel
* `/ignore <nick>` - Toggle ignoring a user by nickname.
* `/join <channel>` - join a new channel (optional key argument)
* `/kick <nick> <msg>` - Kick a user from the current channel
* `/masks <mode>` - Show the bans (b), quiets (q), invex (I), or ban exemptions (e) for a channel. The list must be requested as above.
* `/me <message>` - send an action to the current channel
* `/mode <mode> <arguments>` - Set modes on the current channel
* `/msg <nick> <message>` - send a private message
* `/nick <nick>` - Change your nickname
* `/notice <nick> <message>` - send a notice message
* `/op` - Request ops from chanserv on the current channel
* `/part <message>` - part the current channel with the given message
* `/query <nick>` - switch to a user message window
* `/quote <raw client command>` - send a client command verbatim
* `/remove <nick> <msg>` - Force a user to part from the current channel
* `/server` - switch to the server message window
* `/topic <topic>` - Change the topic for the current channel
* `/umode <mode>` - Set modes on yourself
* `/whois <nick>` - Query the server for information about a user

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
* `^F` forward
* `^B` backward word
* `M-F` forward word
* `M-B` backward word
* `TAB` nickname completion
* `F2` toggle detailed view
* `Page Up` scroll up
* `Page Down` scroll down
