{-# LANGUAGE CPP #-}

-- | Compatibility shim to keep CPP out of my other files
module Irc.Time (myParseTime) where

#if MIN_VERSION_time(1,5,0)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
#else
import Data.Time (UTCTime, parseTime)
import System.Locale (defaultTimeLocale)
#endif

-- | Compatibility indirection for time-1.4.2 and time-1.5 compatibility
myParseTime ::
  String {- ^ Format string -} ->
  String {- ^ Input string  -} ->
  Maybe UTCTime
#if MIN_VERSION_time(1,5,0)
myParseTime = parseTimeM True defaultTimeLocale
#else
myParseTime = parseTime defaultTimeLocale
#endif

{-# INLINE myParseTime #-}
