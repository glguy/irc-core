{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Operator
Description : Operator command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Operator (operatorCommands) where

import Client.Commands.Arguments.Spec (optionalArg, remainingArg, simpleToken)
import Client.Commands.TabCompletion (noNetworkTab, simpleNetworkTab)
import Client.Commands.Types
import Client.State.Network (sendMsg)
import Control.Applicative (liftA2, liftA3)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text qualified as Text
import Irc.Commands
import Irc.RawIrcMsg (rawIrcMsg)

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
      (liftA2 (,) (simpleToken "[user@]host") (optionalArg (simpleToken "[servername]")))
      "Unban a client from the server.\n"
    $ NetworkCommand cmdUnkline simpleNetworkTab

  , Command
      (pure "undline")
      (liftA2 (,) (simpleToken "host") (optionalArg (simpleToken "[servername]")))
      "Unban a client from the server.\n"
    $ NetworkCommand cmdUndline simpleNetworkTab

  , Command
      (pure "unxline")
      (liftA2 (,) (simpleToken "gecos") (optionalArg (simpleToken "[servername]")))
      "Unban a gecos from the server.\n"
    $ NetworkCommand cmdUnxline simpleNetworkTab

  , Command
      (pure "unresv")
      (liftA2 (,) (simpleToken "channel|nick") (optionalArg (simpleToken "[servername]")))
      "Unban a channel or nickname from the server.\n"
    $ NetworkCommand cmdUnresv simpleNetworkTab

  , Command
      (pure "testline")
      (simpleToken "[[nick!]user@]host")
      "Check matching I/K/D lines for a [[nick!]user@]host\n"
    $ NetworkCommand cmdTestline simpleNetworkTab

  , Command
      (pure "testkline")
      (simpleToken "[user@]host")
      "Check matching K/D lines for a [user@]host\n"
    $ NetworkCommand cmdTestkline simpleNetworkTab

  , Command
      (pure "testgecos")
      (simpleToken "gecos")
      "Check matching X lines for a gecos\n"
    $ NetworkCommand cmdTestGecos simpleNetworkTab

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

  , Command
      (pure "sconnect")
      (liftA2 (,) (simpleToken "connect_to") (optionalArg (liftA2 (,) (simpleToken "[port]") (optionalArg (simpleToken "[remote]")))))
      "Connect two servers together.\n"
    $ NetworkCommand cmdSconnect simpleNetworkTab

  , Command
      (pure "squit")
      (liftA2 (,) (simpleToken "server") (remainingArg "[reason]"))
      "Split a server away from your side of the network.\n"
    $ NetworkCommand cmdSquit simpleNetworkTab

  , Command
      (pure "modload")
      (liftA2 (,) (simpleToken "[path/]module") (optionalArg (simpleToken "[remote]")))
      "Load an IRCd module.\n"
    $ NetworkCommand cmdModload simpleNetworkTab

  , Command
      (pure "modunload")
      (liftA2 (,) (simpleToken "module") (optionalArg (simpleToken "[remote]")))
      "Unload an IRCd module.\n"
    $ NetworkCommand cmdModunload simpleNetworkTab

  , Command
      (pure "modlist")
      (optionalArg (liftA2 (,) (simpleToken "pattern") (optionalArg (simpleToken "[remote]"))))
      "List loaded IRCd modules.\n"
    $ NetworkCommand cmdModlist simpleNetworkTab

  , Command
      (pure "modrestart")
      (optionalArg (simpleToken "[server]"))
      "Reload all IRCd modules.\n"
    $ NetworkCommand cmdModrestart simpleNetworkTab

  , Command
      (pure "modreload")
      (liftA2 (,) (simpleToken "module") (optionalArg (simpleToken "[remote]")))
      "Reload an IRCd module.\n"
    $ NetworkCommand cmdModreload simpleNetworkTab

  , Command
      (pure "grant")
      (liftA2 (,) (simpleToken "target") (simpleToken "privset"))
      "Manually assign a privset to a user.\n"
    $ NetworkCommand cmdGrant simpleNetworkTab

  , Command
      (pure "privs")
      (optionalArg (simpleToken "[target]"))
      "Check operator privileges of the target.\n"
    $ NetworkCommand cmdPrivs simpleNetworkTab
  ]

cmdGrant :: NetworkCommand (String, String)
cmdGrant cs st (target, privset) =
  do sendMsg cs (rawIrcMsg "GRANT" (Text.pack <$> [target, privset]))
     commandSuccess st

cmdPrivs :: NetworkCommand (Maybe String)
cmdPrivs cs st mbTarget =
  do sendMsg cs (rawIrcMsg "PRIVS" (Text.pack <$> maybeToList mbTarget))
     commandSuccess st

cmdModlist :: NetworkCommand (Maybe (String, Maybe String))
cmdModlist cs st args =
  do let argList = case args of
                     Nothing -> []
                     Just (x, xs) -> x : maybeToList xs
     sendMsg cs (rawIrcMsg "MODLIST" (Text.pack <$> argList))
     commandSuccess st

cmdModrestart :: NetworkCommand (Maybe String)
cmdModrestart cs st args =
  do sendMsg cs (rawIrcMsg "MODRESTART" (Text.pack <$> maybeToList args))
     commandSuccess st

cmdModload :: NetworkCommand (String, Maybe String)
cmdModload cs st (mod_, remote) =
  do sendMsg cs (rawIrcMsg "MODLOAD" (Text.pack <$> (mod_ : maybeToList remote)))
     commandSuccess st

cmdModunload :: NetworkCommand (String, Maybe String)
cmdModunload cs st (mod_, remote) =
  do sendMsg cs (rawIrcMsg "MODUNLOAD" (Text.pack <$> (mod_ : maybeToList remote)))
     commandSuccess st

cmdModreload :: NetworkCommand (String, Maybe String)
cmdModreload cs st (mod_, remote) =
  do sendMsg cs (rawIrcMsg "MODRELOAD" (Text.pack <$> (mod_ : maybeToList remote)))
     commandSuccess st

cmdSquit :: NetworkCommand (String, String)
cmdSquit cs st (server, reason) =
  do sendMsg cs (rawIrcMsg "SQUIT" (Text.pack <$> [server, reason]))
     commandSuccess st

cmdSconnect :: NetworkCommand (String, Maybe (String, Maybe String))
cmdSconnect cs st (server, rest) =
  do let args = case rest of
                  Nothing -> [server]
                  Just (x, xs) -> server : x : maybeToList xs
     sendMsg cs (rawIrcMsg "CONNECT" (Text.pack <$> args))
     commandSuccess st

cmdKill :: NetworkCommand (String, String)
cmdKill cs st (client,rest) =
  do sendMsg cs (ircKill (Text.pack client) (Text.pack rest))
     commandSuccess st

cmdKline :: NetworkCommand (String, String, String)
cmdKline cs st (minutes, mask, reason) =
  do sendMsg cs (ircKline (Text.pack minutes) (Text.pack mask) (Text.pack reason))
     commandSuccess st

cmdUnkline :: NetworkCommand (String, Maybe String)
cmdUnkline cs st (mask, server) =
  do sendMsg cs (ircUnkline (Text.pack mask) (Text.pack <$> server))
     commandSuccess st

cmdUndline :: NetworkCommand (String, Maybe String)
cmdUndline cs st (mask, server) =
  do sendMsg cs (ircUndline (Text.pack mask) (Text.pack <$> server))
     commandSuccess st

cmdUnxline :: NetworkCommand (String, Maybe String)
cmdUnxline cs st (mask, server) =
  do sendMsg cs (ircUnxline (Text.pack mask) (Text.pack <$> server))
     commandSuccess st

cmdUnresv :: NetworkCommand (String, Maybe String)
cmdUnresv cs st (mask, server) =
  do sendMsg cs (ircUnresv (Text.pack mask) (Text.pack <$> server))
     commandSuccess st

cmdTestline :: NetworkCommand String
cmdTestline cs st mask =
  do sendMsg cs (ircTestline (Text.pack mask))
     commandSuccess st

cmdTestkline :: NetworkCommand String
cmdTestkline cs st mask =
  do sendMsg cs (rawIrcMsg "TESTKLINE" [Text.pack mask])
     commandSuccess st

cmdTestGecos :: NetworkCommand String
cmdTestGecos cs st mask =
  do sendMsg cs (rawIrcMsg "TESTGECOS" [Text.pack mask])
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
