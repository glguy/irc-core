My IRC client
=============

This client has fewer features than the one you're using now. You probably don't want to use it.

Library
=======

This package is split into a generic IRC modeling library and a VTY-base text client using that library.

Startup
=======

glirc <options> SERVER
  -p PORT  --port=PORT  IRC Server Port
  -n NICK  --nick=NICK  Nickname
  -u USER  --user=USER  Username
  -r REAL  --real=REAL  Real Name
  -h       --help       Show help

Commands
========

* `/quote`
* `/msg`
* `/me`
* `/join`
* `/quote`
* `/channel`
* `/channelinfo`
* `/query`
* `/server`
* `/bans`

Keyboard Shortcuts
==================

* `^N` next channel
* `^P` previous channel
* `^A` beginning of line
* `^E` end of line
* `^K` delete to end
* `^U` delete to beginning
* `^D` delete at cursor
* `^W` delete word
* `TAB` nickname completion
* `F2` toggle detailed view
* `Page Up` scroll up
* `Page Down` scroll down
