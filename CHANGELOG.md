1.1.5
-------
* Show effects of control-modifiers inline when composing chat messages
* Show time since last transmitted ping
* Automatically split long chat messages

1.1.4
-------
* Support host-specific server-certificate sections
* Make message parsing more permissive for use with slack irc bridge
* Fix parsing of INVITE reply message

1.1.3
-------
* Support for running commands upon connection
* Support for SOCKS5 proxy
* Merge view of all channels (F5)

1.1.2
-------
* Support multiple nicknames in `/filter`
* Periodically ping to determine ping-times and keep connection alive.
* Added ping time field to IrcConnection datatype.
* Add `/ping` command

1.1.1.1
-------
* Better error handling
* Added a stack.yaml

1.1.1
-----
* Add `/grep` filter command

1.1.0.1
-------
* Fix setting default nick in configuration file
* Dependency version constraint bumps

1.1
---
* Better support for Freenode's trailing spaces
* More compact metadata representation
* Ignored messages no longer count toward unread number
* Updated version bounds on lens and attoparsec
* Channel info has user count

1.0
---
* Initial hackage release
