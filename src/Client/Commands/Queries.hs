{-# Language OverloadedStrings, TemplateHaskell #-}
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

import Client.Commands.Arguments.Spec (optionalArg, remainingArg, simpleToken, tokenArg)
import Client.Commands.Docs (queriesDocs, cmdDoc)
import Client.Commands.TabCompletion (noNetworkTab, simpleNetworkTab)
import Client.Commands.Types (commandSuccess, commandSuccessUpdateCS, Command(Command), CommandImpl(NetworkCommand), CommandSection(CommandSection), NetworkCommand)
import Client.State (changeSubfocus)
import Client.State.Focus (Subfocus(FocusChanList, FocusWho))
import Client.State.Network (sendMsg, csChannelList, clsElist, csPingStatus, _PingConnecting, csWhoReply, csNetwork)
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
      $(queriesDocs `cmdDoc` "who")
    $ NetworkCommand cmdWho simpleNetworkTab

  , Command
      (pure "whois")
      (remainingArg "arguments")
      $(queriesDocs `cmdDoc` "whois")
    $ NetworkCommand cmdWhois simpleNetworkTab

  , Command
      (pure "whowas")
      (remainingArg "arguments")
      $(queriesDocs `cmdDoc` "whowas")
    $ NetworkCommand cmdWhowas simpleNetworkTab

  , Command
      (pure "ison")
      (remainingArg "arguments")
      $(queriesDocs `cmdDoc` "ison")
    $ NetworkCommand cmdIson   simpleNetworkTab

  , Command
      (pure "userhost")
      (remainingArg "arguments")
      $(queriesDocs `cmdDoc` "userhost")
    $ NetworkCommand cmdUserhost simpleNetworkTab

  , Command
      (pure "time")
      (optionalArg (simpleToken "[servername]"))
      $(queriesDocs `cmdDoc` "time")
    $ NetworkCommand cmdTime simpleNetworkTab

  , Command
      (pure "stats")
      (remainingArg "arguments")
      $(queriesDocs `cmdDoc` "stats")
    $ NetworkCommand cmdStats simpleNetworkTab

  , Command
      (pure "lusers")
      (optionalArg (simpleToken "[servername]"))
      $(queriesDocs `cmdDoc` "lusers")
    $ NetworkCommand cmdLusers simpleNetworkTab

  , Command
      (pure "users")
      (optionalArg (simpleToken "[servername]"))
      $(queriesDocs `cmdDoc` "users")
    $ NetworkCommand cmdUsers simpleNetworkTab

  , Command
      (pure "motd")
      (optionalArg (simpleToken "[servername]"))
      $(queriesDocs `cmdDoc` "motd")
    $ NetworkCommand cmdMotd simpleNetworkTab

  , Command
      (pure "admin")
      (optionalArg (simpleToken "[servername]"))
      $(queriesDocs `cmdDoc` "admin")
    $ NetworkCommand cmdAdmin simpleNetworkTab

  , Command
      (pure "rules")
      (optionalArg (simpleToken "[servername]"))
      $(queriesDocs `cmdDoc` "rules")
    $ NetworkCommand cmdRules simpleNetworkTab

  , Command
      (pure "info")
      (pure ())
      $(queriesDocs `cmdDoc` "info")
    $ NetworkCommand cmdInfo noNetworkTab

  , Command
      (pure "list")
      (optionalArg (liftA2 (,) (tokenArg "[clientopts]" (const lsaParse)) (optionalArg (simpleToken "[elist]"))))
      $(queriesDocs `cmdDoc` "list")
    $ NetworkCommand cmdList simpleNetworkTab

  , Command
      (pure "links")
      (remainingArg "arguments")
      $(queriesDocs `cmdDoc` "links")
    $ NetworkCommand cmdLinks simpleNetworkTab

  , Command
      (pure "version")
      (optionalArg (simpleToken "[servername]"))
      $(queriesDocs `cmdDoc` "version")
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

cmdList :: NetworkCommand (Maybe (ListArgs, Maybe String))
cmdList cs st rest =
    do
      let (lsa, maybeElist) = fromMaybe (lsaDefault, Nothing) rest
      let connecting = has (csPingStatus . _PingConnecting) cs
      let elist = Just (Text.pack (fromMaybe "" maybeElist))
      let cached = elist == view (csChannelList . clsElist) cs
      let sendM = sendMsg cs (ircList (Text.pack <$> maybeToList maybeElist))
      unless (connecting || (cached && not (_lsaRefresh lsa))) sendM
      let cs' = set (csChannelList . clsElist) elist cs 
      let subfocus = FocusChanList (view csNetwork cs) (_lsaMin lsa) (_lsaMax lsa)
      commandSuccessUpdateCS cs' (changeSubfocus subfocus st)

data ListArgs = ListArgs
  { _lsaRefresh :: Bool
  , _lsaMin     :: Maybe Int
  , _lsaMax     :: Maybe Int
  }

lsaDefault :: ListArgs
lsaDefault = ListArgs
  { _lsaRefresh = False
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
cmdWho cs  st Nothing = commandSuccess $ changeSubfocus (FocusWho (view csNetwork cs)) st
cmdWho cs st (Just (query, arg)) =
  do
    let query' = Text.pack query
    let arg' = fromMaybe "" arg
    let cs' = set csWhoReply (newWhoReply query' arg') cs
    sendMsg cs (ircWho (query' : maybeToList (Text.pack <$> arg)))
    commandSuccessUpdateCS cs' $ changeSubfocus (FocusWho (view csNetwork cs)) st

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
