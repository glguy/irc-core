{-# Language OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Client.Commands.Queries
Description : Query command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Queries (queryCommands) where

import Client.Commands.Arguments.Spec (optionalArg, remainingArg, simpleToken, extensionArg, Args)
import Client.Commands.TabCompletion (noNetworkTab, simpleNetworkTab)
import Client.Commands.Types (commandSuccess, commandSuccessUpdateCS, Command(Command), CommandImpl(NetworkCommand), CommandSection(CommandSection), NetworkCommand)
import Client.State (changeSubfocus, ClientState)
import Client.State.Focus (Subfocus(FocusChanList, FocusWho))
import Client.State.Network (sendMsg, csChannelList, clsElist, csPingStatus, _PingConnecting, csWhoReply)
import Client.WhoReply (newWhoReply)
import Control.Applicative (liftA2)
import Control.Lens (has, set, view)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text qualified as Text
import Irc.Commands

queryCommands :: CommandSection
queryCommands = CommandSection "Queries"

  [ Command
      (pure "who")
      (optionalArg (liftA2 (,) (simpleToken "[channel|nick|mask]") (optionalArg (simpleToken "[options]"))))
      "Send WHO query to server with given arguments, or just show the who view.\n"
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
      (pure "list")
      (optionalArg (extensionArg "[clientarg]" listArgs))
      "\^BParameters:\^B\n\
      \\n\
      \    clientarg: An optionally-comma-separated list of\n\
      \               flags for controlling the list.\n\
      \        ~: Always refresh the list.\n\
      \        >n: Show only channels with more than \^Bn\^B users.\n\
      \        <n: Show only channels with less than \^Bn\^B users.\n\
      \\n\
      \    serverarg: The ELIST argument to send to the server.\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    View the list of public channels on the server.\n\
      \\n\
      \    Sends a LIST query and caches the result;\n\
      \    on larger networks on slower connections,\n\
      \    this may take a while to complete.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /list\n\
      \    /list >100\n\
      \    /list ~ <20\n\
      \    /list , *-ops"
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

cmdList :: NetworkCommand (Maybe ListArgs)
cmdList cs st rest =
    do
      let lsa = fromMaybe lsaDefault rest
      let connecting = has (csPingStatus . _PingConnecting) cs
      let elist = Just (Text.pack (fromMaybe "" (_lsaElist lsa)))
      let cached = elist == view (csChannelList . clsElist) cs
      let sendM = sendMsg cs (ircList (Text.pack <$> maybeToList (_lsaElist lsa)))
      unless (connecting || (cached && not (_lsaRefresh lsa))) sendM
      let cs' = set (csChannelList . clsElist) elist cs 
      let subfocus = FocusChanList (_lsaMin lsa) (_lsaMax lsa)
      commandSuccessUpdateCS cs' (changeSubfocus subfocus st)

listArgs :: ClientState -> String -> Maybe (Args ClientState ListArgs)
listArgs _ = fmap (withElist (optionalArg (simpleToken "[serverarg]"))) . lsaParse
    where withElist arg a = fmap (\s -> a { _lsaElist = s }) arg

data ListArgs = ListArgs
  { _lsaElist   :: Maybe String
  , _lsaRefresh :: Bool
  , _lsaMin     :: Maybe Int
  , _lsaMax     :: Maybe Int
  }

lsaDefault :: ListArgs
lsaDefault = ListArgs
  { _lsaElist = Nothing
  , _lsaRefresh = False
  , _lsaMin = Nothing
  , _lsaMax = Nothing
  }

lsaParse :: String -> Maybe ListArgs
lsaParse = lsaParse' lsaDefault
  where
    lsaParse' lsa str = case str of
      '~':rest -> lsaParse' lsa{ _lsaRefresh = True } rest
      ',':rest -> lsaParse' lsa rest
      '>':(reads -> [(min', rest)]) | min' >= 0 ->
        lsaParse' lsa{ _lsaMin = Just min'} rest
      '<':(reads -> [(max', rest)]) | max' >= 0 ->
        lsaParse' lsa{ _lsaMax = Just max'} rest
      "" -> Just lsa
      _ -> Nothing

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

cmdWho :: NetworkCommand (Maybe (String, Maybe String))
cmdWho _  st Nothing = commandSuccess (changeSubfocus FocusWho st)
cmdWho cs st (Just (query, arg)) =
  do
    let query' = Text.pack query
    let arg' = fromMaybe "" arg
    let cs' = set csWhoReply (newWhoReply query' arg') cs
    sendMsg cs (ircWho (query' : (maybeToList $ Text.pack <$> arg)))
    commandSuccessUpdateCS cs' (changeSubfocus FocusWho st)

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
