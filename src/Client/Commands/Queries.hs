{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Queries
Description : Query command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Queries (queryCommands) where

import Client.Commands.Arguments.Spec (optionalArg, remainingArg, simpleToken)
import Client.Commands.TabCompletion (noNetworkTab, simpleNetworkTab)
import Client.Commands.Types (commandSuccess, Command(Command), CommandImpl(NetworkCommand), CommandSection(CommandSection), NetworkCommand)
import Client.State.Network (sendMsg)
import Data.Text qualified as Text
import Irc.Commands

queryCommands :: CommandSection
queryCommands = CommandSection "Queries"

  [ Command
      (pure "who")
      (remainingArg "arguments")
      "Send WHO query to server with given arguments.\n"
    $ NetworkCommand cmdWho simpleNetworkTab

  , Command
      (pure "whois")
      (remainingArg "arguments")
      "Send WHOIS query to server with given arguments.\n"
    $ NetworkCommand cmdWhois simpleNetworkTab

  , Command
      (pure "whowas")
      (remainingArg "arguments")
      "Send WHOWAS query to server with given arguments.\n"
    $ NetworkCommand cmdWhowas simpleNetworkTab

  , Command
      (pure "ison")
      (remainingArg "arguments")
      "Send ISON query to server with given arguments.\n"
    $ NetworkCommand cmdIson   simpleNetworkTab

  , Command
      (pure "userhost")
      (remainingArg "arguments")
      "Send USERHOST query to server with given arguments.\n"
    $ NetworkCommand cmdUserhost simpleNetworkTab

  , Command
      (pure "time")
      (optionalArg (simpleToken "[servername]"))
      "Send TIME query to server with given arguments.\n"
    $ NetworkCommand cmdTime simpleNetworkTab

  , Command
      (pure "stats")
      (remainingArg "arguments")
      "Send STATS query to server with given arguments.\n"
    $ NetworkCommand cmdStats simpleNetworkTab

  , Command
      (pure "lusers")
      (optionalArg (simpleToken "[servername]"))
      "Send LUSERS query to a given server.\n"
    $ NetworkCommand cmdLusers simpleNetworkTab

  , Command
      (pure "users")
      (optionalArg (simpleToken "[servername]"))
      "Send USERS query to a given server.\n"
    $ NetworkCommand cmdUsers simpleNetworkTab

  , Command
      (pure "motd") (optionalArg (simpleToken "[servername]"))
      "Send MOTD query to server.\n"
    $ NetworkCommand cmdMotd simpleNetworkTab

  , Command
      (pure "admin") (optionalArg (simpleToken "[servername]"))
      "Send ADMIN query to server.\n"
    $ NetworkCommand cmdAdmin simpleNetworkTab

  , Command
      (pure "rules") (optionalArg (simpleToken "[servername]"))
      "Send RULES query to server.\n"
    $ NetworkCommand cmdRules simpleNetworkTab

  , Command
      (pure "info") (pure ())
      "Send INFO query to server.\n"
    $ NetworkCommand cmdInfo noNetworkTab

  , Command
      (pure "list") (remainingArg "arguments")
      "Send LIST query to server.\n"
    $ NetworkCommand cmdList simpleNetworkTab

  , Command
      (pure "links")
      (remainingArg "arguments")
      "Send LINKS query to server with given arguments.\n"
    $ NetworkCommand cmdLinks simpleNetworkTab

  , Command
      (pure "version") (optionalArg (simpleToken "[servername]"))
      "Send VERSION query to server.\n"
    $ NetworkCommand cmdVersion simpleNetworkTab

  ]

cmdInfo :: NetworkCommand ()
cmdInfo cs st _ =
  do sendMsg cs ircInfo
     commandSuccess st

cmdVersion :: NetworkCommand (Maybe String)
cmdVersion cs st mbservername =
  do sendMsg cs $ ircVersion $ case mbservername of
                                Just s  -> Text.pack s
                                Nothing -> ""
     commandSuccess st

cmdList :: NetworkCommand String
cmdList cs st rest =
  do sendMsg cs (ircList (Text.pack <$> words rest))
     commandSuccess st

cmdLusers :: NetworkCommand (Maybe String)
cmdLusers cs st arg =
  do sendMsg cs $ ircLusers $ fmap Text.pack $
       case arg of
         Nothing -> []
         Just x -> ["*", x] -- mask field is legacy
     commandSuccess st

cmdUsers :: NetworkCommand (Maybe String)
cmdUsers cs st arg =
  do sendMsg cs $ ircUsers $ maybe "" Text.pack arg
     commandSuccess st

cmdMotd :: NetworkCommand (Maybe String)
cmdMotd cs st mbservername =
  do sendMsg cs $ ircMotd $ case mbservername of
                              Just s  -> Text.pack s
                              Nothing -> ""
     commandSuccess st

cmdAdmin :: NetworkCommand (Maybe String)
cmdAdmin cs st mbservername =
  do sendMsg cs $ ircAdmin $ case mbservername of
                              Just s  -> Text.pack s
                              Nothing -> ""
     commandSuccess st

cmdRules :: NetworkCommand (Maybe String)
cmdRules cs st mbservername =
  do sendMsg cs $ ircRules $
       case mbservername of
         Just s  -> Text.pack s
         Nothing -> ""
     commandSuccess st

cmdStats :: NetworkCommand String
cmdStats cs st rest =
  do sendMsg cs (ircStats (Text.pack <$> words rest))
     commandSuccess st

cmdLinks :: NetworkCommand String
cmdLinks cs st rest =
  do sendMsg cs (ircLinks (Text.pack <$> words rest))
     commandSuccess st

cmdTime :: NetworkCommand (Maybe String)
cmdTime cs st arg =
  do sendMsg cs (ircTime (maybe "" Text.pack arg))
     commandSuccess st

cmdWhois :: NetworkCommand String
cmdWhois cs st rest =
  do sendMsg cs (ircWhois (Text.pack <$> words rest))
     commandSuccess st

cmdWho :: NetworkCommand String
cmdWho cs st rest =
  do sendMsg cs (ircWho (Text.pack <$> words rest))
     commandSuccess st

cmdWhowas :: NetworkCommand String
cmdWhowas cs st rest =
  do sendMsg cs (ircWhowas (Text.pack <$> words rest))
     commandSuccess st

cmdIson :: NetworkCommand String
cmdIson cs st rest =
  do sendMsg cs (ircIson (Text.pack <$> words rest))
     commandSuccess st

cmdUserhost :: NetworkCommand String
cmdUserhost cs st rest =
  do sendMsg cs (ircUserhost (Text.pack <$> words rest))
     commandSuccess st
