{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Operator
Description : Operator command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Operator (operatorCommands) where

import           Control.Applicative
import           Client.Commands.Arguments.Spec
import           Client.Commands.TabCompletion
import           Client.Commands.Types
import           Client.State.Network (sendMsg)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import           Irc.Commands

operatorCommands :: CommandSection
operatorCommands = CommandSection "Network operator commands"

  [ Command
      (pure "oper")
      (liftA2 (,) (simpleToken "user") (simpleToken "password"))
      "Authenticate as a server operator.\n"
    $ NetworkCommand cmdOper noNetworkTab

  , Command
      (pure "kill")
      (liftA2 (,) (simpleToken "client") (remainingArg "reason"))
      "Kill a client connection to the server.\n"
    $ NetworkCommand cmdKill simpleNetworkTab

  , Command
      (pure "kline")
      (liftA3 (,,) (simpleToken "minutes") (simpleToken "user@host") (remainingArg "reason"))
      "Ban a client from the server.\n"
    $ NetworkCommand cmdKline simpleNetworkTab

  , Command
      (pure "unkline")
      (simpleToken "user@host")
      "Unban a client from the server.\n"
    $ NetworkCommand cmdUnkline simpleNetworkTab

  , Command
      (pure "testline")
      (simpleToken "[[nick!]user@]host")
      "Check matching I/K/D lines for a [[nick!]user@]host\n"
    $ NetworkCommand cmdTestline simpleNetworkTab

  , Command
      (pure "testmask")
      (liftA2 (,) (simpleToken "[nick!]user@host") (remainingArg "[gecos]"))
      "Test how many local and global clients match a mask.\n"
    $ NetworkCommand cmdTestmask simpleNetworkTab

  , Command
      (pure "masktrace")
      (liftA2 (,) (simpleToken "[nick!]user@host") (remainingArg "[gecos]"))
      "Outputs a list of local users matching the given masks.\n"
    $ NetworkCommand cmdMasktrace simpleNetworkTab

  , Command
      (pure "chantrace")
      (simpleToken "channel")
      "Outputs a list of channel members in etrace format.\n"
    $ NetworkCommand cmdChantrace simpleNetworkTab

  , Command
      (pure "trace")
      (optionalArg (liftA2 (,) (simpleToken "[server|nick]") (optionalArg (simpleToken "[location]"))))
      "Outputs a list users on a server.\n"
    $ NetworkCommand cmdTrace simpleNetworkTab

  , Command
      (pure "etrace")
      (optionalArg (simpleToken "[-full|-v4|-v6|nick]"))
      "Outputs a list users on a server.\n"
    $ NetworkCommand cmdEtrace simpleNetworkTab

  , Command
      (pure "map")
      (pure ())
      "Display network map.\n"
    $ NetworkCommand cmdMap simpleNetworkTab

  ]

cmdKill :: NetworkCommand (String, String)
cmdKill cs st (client,rest) =
  do sendMsg cs (ircKill (Text.pack client) (Text.pack rest))
     commandSuccess st

cmdKline :: NetworkCommand (String, String, String)
cmdKline cs st (minutes, mask, reason) =
  do sendMsg cs (ircKline (Text.pack minutes) (Text.pack mask) (Text.pack reason))
     commandSuccess st

cmdUnkline :: NetworkCommand String
cmdUnkline cs st mask =
  do sendMsg cs (ircUnkline (Text.pack mask))
     commandSuccess st

cmdTestline :: NetworkCommand String
cmdTestline cs st mask =
  do sendMsg cs (ircTestline (Text.pack mask))
     commandSuccess st

cmdTestmask :: NetworkCommand (String, String)
cmdTestmask cs st (mask, gecos) =
  do sendMsg cs (ircTestmask (Text.pack mask) (Text.pack gecos))
     commandSuccess st

cmdMasktrace :: NetworkCommand (String, String)
cmdMasktrace cs st (mask, gecos) =
  do sendMsg cs (ircMasktrace (Text.pack mask) (Text.pack gecos))
     commandSuccess st

cmdChantrace :: NetworkCommand String
cmdChantrace cs st chan =
  do sendMsg cs (ircChantrace (Text.pack chan))
     commandSuccess st

cmdEtrace :: NetworkCommand (Maybe String)
cmdEtrace cs st arg =
  do sendMsg cs (ircEtrace (Text.pack (fromMaybe "" arg)))
     commandSuccess st

cmdTrace :: NetworkCommand (Maybe (String, Maybe String))
cmdTrace cs st args =
  do let argsList =
           case args of
            Nothing           -> []
            Just (x, Nothing) -> [x]
            Just (x, Just y)  -> [x, y]
     sendMsg cs (ircTrace (map Text.pack argsList))
     commandSuccess st

cmdMap :: NetworkCommand ()
cmdMap cs st _ =
  do sendMsg cs ircMap
     commandSuccess st

cmdOper :: NetworkCommand (String, String)
cmdOper cs st (user, pass) =
  do sendMsg cs (ircOper (Text.pack user) (Text.pack pass))
     commandSuccess st
