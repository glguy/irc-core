# Revision history for irc-core

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
