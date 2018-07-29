# Revision history for irc-core

## 2.4.0

* Change TOPIC and INVITING replies to channel target

## 2.3.0 -- 2017-06-02

* Change type of `idDenote` to save a bit of memory
* Add more commands to `Irc.Commands`
* Fix comments

## 2.2.1 -- 2017-05-13

* Prettier reply code text

## 2.2.0.1  -- 2016-12-18

* Exchange `memory` dependency for `base64-bytestring`

## 2.2.0.0  -- 2016-09-15

* `ircIson` packs all the nicks into a single argument
* `Eq` and `Ord` instances for `ReplyCode`
* Use `Text` for nickname targets in Irc.Commands

## 2.1.1.1  -- 2016-08-28

* Added parsing tests
* Slightly more tolerant of whitespace

## 2.1.1.0  -- 2016-08-13

* Add `Eq` instances to `UserInfo` and `RawIrcMsg`
* Add `IsString` instance to `Identifier`
* Remove `lens` dependency (functionality preserved)
* Show and Read instances for `Identifier` render the text version as a string literal

## 2.1.0.0  -- 2016-08-13

* Add BatchStart and BatchEnd messages

## 2.0.0.0  -- 2016-08-08

* Extracted from glirc-2.5
