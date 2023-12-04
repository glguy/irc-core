{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.ZNC
Description : ZNC command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.ZNC (zncCommands) where

import Control.Applicative ((<|>), empty, liftA2)
import Client.Commands.Arguments.Spec (optionalArg, remainingArg, simpleToken)
import Client.Commands.TabCompletion (noNetworkTab, simpleNetworkTab)
import Client.Commands.Types
import Client.State.Network (sendMsg)
import Data.Text qualified as Text
import Data.Time
import Irc.Commands (ircZnc)
import Control.Lens ((<<.~), (??), over)
import LensUtils (localTimeDay, localTimeTimeOfDay, zonedTimeLocalTime)

zncCommands :: CommandSection
zncCommands = CommandSection "ZNC Support"

  [ Command
      (pure "znc")
      (remainingArg "arguments")
      "Send command directly to ZNC.\n\
      \\n\
      \The advantage of this over /msg is that responses are not broadcast to call clients.\n"
    $ NetworkCommand cmdZnc simpleNetworkTab

  , Command
      (pure "znc-playback")
      (optionalArg (liftA2 (,) (simpleToken "[time]") (optionalArg (simpleToken "[date]"))))
      "Request playback from the ZNC 'playback' module.\n\
      \\n\
      \\^Btime\^B determines the time to playback since.\n\
      \\^Bdate\^B determines the date to playback since.\n\
      \\n\
      \When both \^Btime\^B and \^Bdate\^B are omitted, all playback is requested.\n\
      \When both \^Bdate\^B is omitted it is defaulted the most recent date in the past that makes sense.\n\
      \\n\
      \Time format: HOURS:MINUTES (example: 7:00)\n\
      \Date format: YEAR-MONTH-DAY (example: 2016-06-16)\n\
      \\n\
      \Note that the playback module is not installed in ZNC by default!\n"
    $ NetworkCommand cmdZncPlayback noNetworkTab

  ]

cmdZnc :: NetworkCommand String
cmdZnc cs st rest =
  do sendMsg cs (ircZnc (Text.words (Text.pack rest)))
     commandSuccess st

cmdZncPlayback :: NetworkCommand (Maybe (String, Maybe String))
cmdZncPlayback cs st args =
  case args of

    -- request everything
    Nothing -> success "0"

    -- current date explicit time
    Just (timeStr, Nothing)
       | Just tod <- parseFormats timeFormats timeStr ->
          do now <- getZonedTime
             let (nowTod,t) = (zonedTimeLocalTime . localTimeTimeOfDay <<.~ tod) now
                 yesterday = over (zonedTimeLocalTime . localTimeDay) (addDays (-1))
                 fixDay
                   | tod <= nowTod = id
                   | otherwise     = yesterday
             successZoned (fixDay t)

    -- explicit date and time
    Just (timeStr, Just dateStr)
       | Just day  <- parseFormats dateFormats dateStr
       , Just tod  <- parseFormats timeFormats timeStr ->
          do tz <- getCurrentTimeZone
             successZoned ZonedTime
               { zonedTimeZone = tz
               , zonedTimeToLocalTime = LocalTime
                   { localTimeOfDay = tod
                   , localDay       = day } }

    _ -> commandFailureMsg "unable to parse date/time arguments" st

  where
    -- %k doesn't require a leading 0 for times before 10AM
    timeFormats = ["%k:%M:%S","%k:%M"]
    dateFormats = ["%F"]
    parseFormats formats str =
      -- asum requires base >= 4.16
      foldr (<|>) empty (map (parseTimeM False defaultTimeLocale ?? str) formats)

    successZoned = success . formatTime defaultTimeLocale "%s"

    success start =
      do sendMsg cs (ircZnc ["*playback", "play", "*", Text.pack start])
         commandSuccess st
