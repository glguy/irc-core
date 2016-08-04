# Revision history for glirc2

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
