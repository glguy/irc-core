{-# Language OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Client.Commands.Operator
Description : Operator command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Operator (operatorCommands) where

import Client.Commands.Arguments.Spec (optionalArg, remainingArg, simpleToken)
import Client.Commands.Docs (operDocs, cmdDoc)
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
      $(operDocs `cmdDoc` "oper")
    $ NetworkCommand cmdOper noNetworkTab

  , Command
      (pure "kill")
      (liftA2 (,) (simpleToken "client") (remainingArg "reason"))
      $(operDocs `cmdDoc` "kill")
    $ NetworkCommand cmdKill simpleNetworkTab

  , Command
      (pure "kline")
      (liftA3 (,,) (simpleToken "minutes") (simpleToken "user@host") (remainingArg "reason"))
      $(operDocs `cmdDoc` "kline")
    $ NetworkCommand cmdKline simpleNetworkTab

  , Command
      (pure "unkline")
      (liftA2 (,) (simpleToken "[user@]host") (optionalArg (simpleToken "[servername]")))
      $(operDocs `cmdDoc` "unkline")
    $ NetworkCommand cmdUnkline simpleNetworkTab

  , Command
      (pure "undline")
      (liftA2 (,) (simpleToken "host") (optionalArg (simpleToken "[servername]")))
      $(operDocs `cmdDoc` "undline")
    $ NetworkCommand cmdUndline simpleNetworkTab

  , Command
      (pure "unxline")
      (liftA2 (,) (simpleToken "gecos") (optionalArg (simpleToken "[servername]")))
      $(operDocs `cmdDoc` "unxline")
    $ NetworkCommand cmdUnxline simpleNetworkTab

  , Command
      (pure "unresv")
      (liftA2 (,) (simpleToken "channel|nick") (optionalArg (simpleToken "[servername]")))
      $(operDocs `cmdDoc` "unresv")
    $ NetworkCommand cmdUnresv simpleNetworkTab

  , Command
      (pure "testline")
      (simpleToken "[[nick!]user@]host")
      $(operDocs `cmdDoc` "testline")
    $ NetworkCommand cmdTestline simpleNetworkTab

  , Command
      (pure "testkline")
      (simpleToken "[user@]host")
      $(operDocs `cmdDoc` "testkline")
    $ NetworkCommand cmdTestkline simpleNetworkTab

  , Command
      (pure "testgecos")
      (simpleToken "gecos")
      $(operDocs `cmdDoc` "testgecos")
    $ NetworkCommand cmdTestGecos simpleNetworkTab

  , Command
      (pure "testmask")
      (liftA2 (,) (simpleToken "[nick!]user@host") (remainingArg "[gecos]"))
      $(operDocs `cmdDoc` "testmask")
    $ NetworkCommand cmdTestmask simpleNetworkTab

  , Command
      (pure "masktrace")
      (liftA2 (,) (simpleToken "[nick!]user@host") (remainingArg "[gecos]"))
      $(operDocs `cmdDoc` "masktrace")
    $ NetworkCommand cmdMasktrace simpleNetworkTab

  , Command
      (pure "chantrace")
      (simpleToken "channel")
      $(operDocs `cmdDoc` "chantrace")
    $ NetworkCommand cmdChantrace simpleNetworkTab

  , Command
      (pure "trace")
      (optionalArg (liftA2 (,) (simpleToken "[server|nick]") (optionalArg (simpleToken "[location]"))))
      $(operDocs `cmdDoc` "trace")
    $ NetworkCommand cmdTrace simpleNetworkTab

  , Command
      (pure "etrace")
      (optionalArg (simpleToken "[-full|-v4|-v6|nick]"))
      $(operDocs `cmdDoc` "etrace")
    $ NetworkCommand cmdEtrace simpleNetworkTab

  , Command
      (pure "map")
      (pure ())
      $(operDocs `cmdDoc` "map")
    $ NetworkCommand cmdMap simpleNetworkTab

  , Command
      (pure "sconnect")
      (liftA2 (,) (simpleToken "connect_to") (optionalArg (liftA2 (,) (simpleToken "[port]") (optionalArg (simpleToken "[remote]")))))
      $(operDocs `cmdDoc` "sconnect")
    $ NetworkCommand cmdSconnect simpleNetworkTab

  , Command
      (pure "squit")
      (liftA2 (,) (simpleToken "server") (remainingArg "[reason]"))
      $(operDocs `cmdDoc` "squit")
    $ NetworkCommand cmdSquit simpleNetworkTab

  , Command
      (pure "modload")
      (liftA2 (,) (simpleToken "[path/]module") (optionalArg (simpleToken "[remote]")))
      $(operDocs `cmdDoc` "modload")
    $ NetworkCommand cmdModload simpleNetworkTab

  , Command
      (pure "modunload")
      (liftA2 (,) (simpleToken "module") (optionalArg (simpleToken "[remote]")))
      $(operDocs `cmdDoc` "modunload")
    $ NetworkCommand cmdModunload simpleNetworkTab

  , Command
      (pure "modlist")
      (optionalArg (liftA2 (,) (simpleToken "pattern") (optionalArg (simpleToken "[remote]"))))
      $(operDocs `cmdDoc` "modlist")
    $ NetworkCommand cmdModlist simpleNetworkTab

  , Command
      (pure "modrestart")
      (optionalArg (simpleToken "[server]"))
      $(operDocs `cmdDoc` "modrestart")
    $ NetworkCommand cmdModrestart simpleNetworkTab

  , Command
      (pure "modreload")
      (liftA2 (,) (simpleToken "module") (optionalArg (simpleToken "[remote]")))
      $(operDocs `cmdDoc` "modreload")
    $ NetworkCommand cmdModreload simpleNetworkTab

  , Command
      (pure "grant")
      (liftA2 (,) (simpleToken "target") (simpleToken "privset"))
      $(operDocs `cmdDoc` "grant")
    $ NetworkCommand cmdGrant simpleNetworkTab

  , Command
      (pure "privs")
      (optionalArg (simpleToken "[target]"))
      $(operDocs `cmdDoc` "privs")
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
