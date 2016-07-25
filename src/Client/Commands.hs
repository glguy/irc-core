{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Client.Commands
Description : Implementation of slash commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel mask list. A mask list
can show channel bans, quiets, invites, and exceptions.
-}

module Client.Commands
  ( CommandResult(..)
  , executeCommand
  , nickTabCompletion
  ) where

import           Client.ConnectionState
import           Client.Message
import           Client.ServerSettings
import           Client.ChannelState
import           Client.State
import           Client.Window
import           Client.WordCompletion
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.List.Split
import           Data.Foldable
import           Data.HashSet (HashSet)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))
import           Data.Time
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.Message
import           Irc.UserInfo
import           Irc.Modes
import           LensUtils
import qualified Client.EditBox as Edit

-- | Possible results of running a command
data CommandResult
  = CommandContinue ClientState -- ^ Continue running client with updated state
  | CommandQuit -- ^ Client should close

type ClientCommand = ClientState -> String -> IO CommandResult
type NetworkCommand = NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
type ChannelCommand = NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult

-- | Pair of implementations for executing a command and tab completing one.
-- The tab-completion logic is extended with a bool
-- indicating that tab completion should be reversed
data Command
  = ClientCommand  ClientCommand  (Bool -> ClientCommand)
  | NetworkCommand NetworkCommand (Bool -> NetworkCommand)
  | ChannelCommand ChannelCommand (Bool -> ChannelCommand)


commandContinue :: Monad m => ClientState -> m CommandResult
commandContinue = return . CommandContinue

splitWord :: String -> (String, String)
splitWord str = (w, drop 1 rest)
  where
    (w, rest) = break isSpace str

nextWord :: String -> (String, String)
nextWord = splitWord . dropWhile isSpace

-- | Parse and execute the given command. When the first argument is Nothing
-- the command is executed, otherwise the first argument is the cursor
-- position for tab-completion
executeCommand :: Maybe Bool -> String -> ClientState -> IO CommandResult

executeCommand (Just isReversed) _ st
  | Just st' <- commandNameCompletion isReversed st = commandContinue st'

executeCommand tabCompleteReversed str st =
  let (cmd, rest) = splitWord str
      cmdTxt      = Text.toLower (Text.pack cmd) in
  case HashMap.lookup cmdTxt commands of

    Just (ClientCommand exec tab) ->
          maybe exec tab tabCompleteReversed
            st rest

    Just (NetworkCommand exec tab)
      | Just network <- views clientFocus focusNetwork st
      , Just cs      <- preview (clientConnection network) st ->
          maybe exec tab tabCompleteReversed
            network cs st rest

    Just (ChannelCommand exec tab)
      | ChannelFocus network channelId <- view clientFocus st
      , Just cs <- preview (clientConnection network) st
      , isChannelIdentifier cs channelId ->
          maybe exec tab tabCompleteReversed
            network cs channelId st rest

    _ -> case tabCompleteReversed of
           Nothing         -> commandContinue st
           Just isReversed -> commandContinue (nickTabCompletion isReversed st)

commands :: HashMap Text Command
commands = HashMap.fromList
  [ ("connect"   , ClientCommand cmdConnect noClientTab)
  , ("exit"      , ClientCommand cmdExit    noClientTab)
  , ("focus"     , ClientCommand cmdFocus   simpleClientTab)
  , ("clear"     , ClientCommand cmdClear   noClientTab)
  , ("reconnect" , ClientCommand cmdReconnect noClientTab)
  , ("ignore"    , ClientCommand cmdIgnore simpleClientTab)

  , ("quote"     , NetworkCommand cmdQuote  simpleNetworkTab)
  , ("join"      , NetworkCommand cmdJoin   simpleNetworkTab)
  , ("mode"      , NetworkCommand cmdMode   simpleNetworkTab)
  , ("msg"       , NetworkCommand cmdMsg    simpleNetworkTab)
  , ("nick"      , NetworkCommand cmdNick   simpleNetworkTab)
  , ("quit"      , NetworkCommand cmdQuit   simpleNetworkTab)
  , ("disconnect", NetworkCommand cmdDisconnect noNetworkTab)
  , ("who"       , NetworkCommand cmdWho    simpleNetworkTab)
  , ("whois"     , NetworkCommand cmdWhois  simpleNetworkTab)
  , ("whowas"    , NetworkCommand cmdWhowas simpleNetworkTab)

  , ("invite"    , ChannelCommand cmdInvite simpleChannelTab)
  , ("topic"     , ChannelCommand cmdTopic  tabTopic    )
  , ("kick"      , ChannelCommand cmdKick   simpleChannelTab)
  , ("remove"    , ChannelCommand cmdRemove simpleChannelTab)
  , ("me"        , ChannelCommand cmdMe     simpleChannelTab)
  , ("part"      , ChannelCommand cmdPart   simpleChannelTab)

  , ("users"     , ChannelCommand cmdUsers  noChannelTab)
  , ("masks"     , ChannelCommand cmdMasks  noChannelTab)
  ]

noClientTab :: Bool -> ClientCommand
noClientTab _ st _ = commandContinue st

noNetworkTab :: Bool -> NetworkCommand
noNetworkTab _ _ _ st _ = commandContinue st

noChannelTab :: Bool -> ChannelCommand
noChannelTab _ _ _ _ st _ = commandContinue st

simpleClientTab :: Bool -> ClientCommand
simpleClientTab isReversed st _ =
  commandContinue (nickTabCompletion isReversed st)

simpleNetworkTab :: Bool -> NetworkCommand
simpleNetworkTab isReversed _ _ st _ =
  commandContinue (nickTabCompletion isReversed st)

simpleChannelTab :: Bool -> ChannelCommand
simpleChannelTab isReversed _ _ _ st _ =
  commandContinue (nickTabCompletion isReversed st)

cmdExit :: ClientState -> String -> IO CommandResult
cmdExit _ _ = return CommandQuit

-- | When used on a channel that the user is currently
-- joined to this command will clear the messages but
-- preserve the window. When used on a window that the
-- user is not joined to this command will delete the window.
cmdClear :: ClientState -> String -> IO CommandResult
cmdClear st _
  = commandContinue
  $ windowEffect
  $ consumeInput st
  where
    windowEffect
      | isActive  = clearWindow
      | otherwise = deleteWindow

    deleteWindow = advanceFocus . setWindow Nothing
    clearWindow  =                setWindow (Just emptyWindow)

    setWindow = set (clientWindows . at (view clientFocus st))

    isActive =
      case view clientFocus st of
        Unfocused -> False
        NetworkFocus network ->
            has (clientConnection network) st
        ChannelFocus network channel ->
            has ( clientConnection network
                . csChannels . ix channel) st


cmdQuote :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdQuote _ cs st rest =
  case parseRawIrcMsg (Text.pack rest) of
    Nothing  -> commandContinue st
    Just raw ->
      do sendMsg cs raw
         commandContinue (consumeInput st)

-- | Implementation of @/me@
cmdMe :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdMe network cs channelId st rest =
  do now <- getZonedTime
     let actionTxt = Text.pack ("\^AACTION " ++ rest ++ "\^A")
         myNick = UserInfo (view csNick cs) Nothing Nothing
         entry = ClientMessage
                    { _msgTime = now
                    , _msgNetwork = network
                    , _msgBody = IrcBody (Action myNick channelId (Text.pack rest))
                    }
     sendMsg cs (rawIrcMsg "PRIVMSG" [idText channelId, actionTxt])
     commandContinue
       $ recordChannelMessage network channelId entry
       $ consumeInput st

-- | Implementation of @/msg@
cmdMsg :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdMsg network cs st rest =
  do now <- getZonedTime
     let (targetsStr, msgStr) = nextWord rest
         targetTxts = Text.split (==',') (Text.pack targetsStr)
         targetIds  = mkId <$> targetTxts
         msgTxt = Text.pack msgStr
         myNick = UserInfo (view csNick cs) Nothing Nothing
         entries = [ (targetId,
                      ClientMessage
                      { _msgTime = now
                      , _msgNetwork = network
                      , _msgBody = IrcBody (Privmsg myNick targetId msgTxt)
                      })
                   | targetId <- targetIds ]

     for_ targetTxts $ \targetTxt ->
       sendMsg cs (rawIrcMsg "PRIVMSG" [targetTxt, msgTxt])

     let st' = foldl' (\acc (targetId, entry) ->
                         recordChannelMessage network targetId entry acc)
                      st
                      entries

     commandContinue (consumeInput st')

cmdConnect :: ClientState -> String -> IO CommandResult
cmdConnect st rest =
  case words rest of
    [networkStr] ->
      do -- abort any existing connection before connecting
         let network = Text.pack networkStr
         st' <- addConnection network =<< abortNetwork network st
         commandContinue
           $ changeFocus (NetworkFocus network)
           $ consumeInput st'

    _ -> commandContinue st

cmdFocus :: ClientState -> String -> IO CommandResult
cmdFocus st rest =
  case words rest of
    [network] ->
      let focus = NetworkFocus (Text.pack network) in
      commandContinue
        $ changeFocus focus
        $ consumeInput st

    [network,channel] ->
      let focus = ChannelFocus (Text.pack network) (mkId (Text.pack channel)) in
      commandContinue
        $ changeFocus focus
        $ consumeInput st

    _ -> commandContinue st

cmdWhois :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdWhois _ cs st rest =
  do sendMsg cs (rawIrcMsg "WHOIS" (Text.pack <$> words rest))
     commandContinue (consumeInput st)

cmdWho :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdWho _ cs st rest =
  do sendMsg cs (rawIrcMsg "WHO" (Text.pack <$> words rest))
     commandContinue (consumeInput st)

cmdWhowas :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdWhowas _ cs st rest =
  do sendMsg cs (rawIrcMsg "WHOWAS" (Text.pack <$> words rest))
     commandContinue (consumeInput st)

cmdMode :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdMode _ cs st rest = modeCommand (Text.pack <$> words rest) cs st

cmdNick :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdNick _ cs st rest =
  case words rest of
    [nick] ->
      do sendMsg cs (rawIrcMsg "NICK" [Text.pack nick])
         commandContinue (consumeInput st)
    _ -> commandContinue st

cmdPart :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdPart _ cs channelId st rest =
  do let msgs = case dropWhile isSpace rest of
                  ""  -> []
                  msg -> [Text.pack msg]
     sendMsg cs (rawIrcMsg "PART" (idText channelId : msgs))
     commandContinue (consumeInput st)

cmdInvite :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdInvite _ cs channelId st rest =
  case words rest of
    [nick] ->
      do let freeTarget = has (csChannels . ix channelId . chanModes . ix 'g') cs
             cmd = rawIrcMsg "INVITE" [Text.pack nick, idText channelId]
         cs' <- if freeTarget
                  then cs <$ sendMsg cs cmd
                  else sendModeration channelId [cmd] cs
         commandContinueUpdateCS cs' st

    _ -> commandContinue st

commandContinueUpdateCS :: ConnectionState -> ClientState -> IO CommandResult
commandContinueUpdateCS cs st =
  let networkId = view csNetworkId cs in
  commandContinue
    $ setStrict (clientConnections . ix networkId) cs
    $ consumeInput st

cmdTopic :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdTopic _ cs channelId st rest =
  do let cmd =
           case dropWhile isSpace rest of
             ""    -> rawIrcMsg "TOPIC" [idText channelId]
             topic | useChanServ channelId cs ->
                        rawIrcMsg "PRIVMSG" ["ChanServ", "TOPIC " <> idText channelId <> Text.pack (' ' : topic)]
                   | otherwise -> rawIrcMsg "TOPIC" [idText channelId, Text.pack topic]
     sendMsg cs cmd
     commandContinue (consumeInput st)

tabTopic :: Bool -> NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
tabTopic _ _ cs channelId st rest

  | all isSpace rest
  , Just topic <- preview (csChannels . ix channelId . chanTopic) cs =
     do let textBox = Edit.end
                    . set Edit.content ("/topic " ++ Text.unpack topic)
        commandContinue (over clientTextBox textBox st)

  | otherwise = commandContinue st

cmdUsers :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdUsers _ _ _ st _ = commandContinue
                    $ changeSubfocus FocusUsers
                    $ consumeInput st

cmdMasks :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdMasks _ cs _ st rest =
  case words rest of
    [[mode]] | mode `elem` view (csModeTypes . modesLists) cs ->
        commandContinue $ changeSubfocus (FocusMasks mode)
                        $ consumeInput st
    _ -> commandContinue st

cmdKick :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdKick _ cs channelId st rest =
  do let (who,reason) = nextWord rest
         msgs = case dropWhile isSpace reason of
                  "" -> []
                  msg -> [Text.pack msg]
     let cmd = rawIrcMsg "KICK" (idText channelId : Text.pack who : msgs)
     cs' <- sendModeration channelId [cmd] cs
     commandContinueUpdateCS cs' st

cmdRemove :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdRemove _ cs channelId st rest =
  do let (who,reason) = nextWord rest
         msgs = case dropWhile isSpace reason of
                  "" -> []
                  msg -> [Text.pack msg]

     let cmd = rawIrcMsg "REMOVE" (idText channelId : Text.pack who : msgs)
     cs' <- sendModeration channelId [cmd] cs
     commandContinueUpdateCS cs' st

cmdJoin :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdJoin network cs st rest =
  let ws = words rest
      doJoin channelTxt =
        do let channelId = mkId (Text.pack (takeWhile (/=',') channelTxt))
           sendMsg cs $ rawIrcMsg "JOIN" (Text.pack <$> ws)
           commandContinue
               $ changeFocus (ChannelFocus network channelId)
               $ consumeInput st
  in case ws of
       [channelTxt]   -> doJoin channelTxt
       [channelTxt,_] -> doJoin channelTxt
       _ -> commandContinue st


cmdQuit :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdQuit _ cs st rest =
  do let msgs = case dropWhile isSpace rest of
                  ""  -> []
                  msg -> [Text.pack msg]
     sendMsg cs (rawIrcMsg "QUIT" msgs)
     commandContinue (consumeInput st)

cmdDisconnect :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdDisconnect network _ st _ =
  do st' <- abortNetwork network st
     commandContinue (consumeInput st')

-- | Reconnect to the currently focused network. It's possible
-- that we're not currently connected to a network, so
-- this is implemented as a client command.
cmdReconnect :: ClientState -> String -> IO CommandResult
cmdReconnect st _
  | Just network <- views clientFocus focusNetwork st =

      do st' <- addConnection network =<< abortNetwork network st
         commandContinue
           $ changeFocus (NetworkFocus network)
           $ consumeInput st'

  | otherwise = commandContinue st

cmdIgnore :: ClientState -> String -> IO CommandResult
cmdIgnore st rest =
  case mkId . Text.pack <$> words rest of
    [] -> commandContinue st
    xs -> commandContinue
            $ over clientIgnores updateIgnores
            $ consumeInput st
      where
        updateIgnores :: HashSet Identifier -> HashSet Identifier
        updateIgnores s = foldl' updateIgnore s xs
        updateIgnore s x = over (contains x) not s

modeCommand :: [Text] -> ConnectionState -> ClientState -> IO CommandResult
modeCommand modes cs st =
  case view clientFocus st of

    NetworkFocus _ ->
      do sendMsg cs (rawIrcMsg "MODE" (idText (view csNick cs) : modes))
         commandContinue (consumeInput st)

    ChannelFocus _ chan ->
      case modes of
        [] -> success False [[]]
        flags:params ->
          case splitModes (view csModeTypes cs) flags params of
            Nothing -> commandContinue st
            Just parsedModes ->
              success needOp (unsplitModes <$> chunksOf (view csModeCount cs) parsedModes')
              where
                parsedModes'
                  | useChanServ chan cs = filter (not . isOpMe) parsedModes
                  | otherwise           = parsedModes

                needOp = not (all isPublicChannelMode parsedModes)
      where
        isOpMe (True, 'o', param) = mkId param == view csNick cs
        isOpMe _                  = False

        success needOp argss =
          do let cmds = [ rawIrcMsg "MODE" (idText chan : args) | args <- argss ]
             cs' <- if needOp
                      then sendModeration chan cmds cs
                      else cs <$ traverse_ (sendMsg cs) cmds
             commandContinueUpdateCS cs' st

    _ -> commandContinue st

-- | Predicate for mode commands that can be performed without ops
isPublicChannelMode :: (Bool, Char, Text) -> Bool
isPublicChannelMode (True, 'b', param) = Text.null param -- query ban list
isPublicChannelMode (True, 'q', param) = Text.null param -- query quiet list
isPublicChannelMode _                  = False

commandNameCompletion :: Bool -> ClientState -> Maybe ClientState
commandNameCompletion isReversed st =
  do guard (cursorPos == n)
     clientTextBox (wordComplete id isReversed possibilities) st
  where
    n = length leadingPart
    leadingPart = takeWhile (not . isSpace) (clientInput st)
    cursorPos   = view (clientTextBox . Edit.pos) st
    possibilities = mkId . Text.cons '/' <$> HashMap.keys commands

-- | Complete the nickname at the current cursor position using the
-- userlist for the currently focused channel (if any)
nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> ClientState
nickTabCompletion isReversed st
  = fromMaybe st
  $ clientTextBox (wordComplete (++": ") isReversed completions) st
  where
    completions = currentUserList st

sendModeration :: Identifier -> [RawIrcMsg] -> ConnectionState -> IO ConnectionState
sendModeration channel cmds cs
  | useChanServ channel cs =
      do sendMsg cs (rawIrcMsg "PRIVMSG" ["ChanServ", "OP", idText channel])
         return $ csChannels . ix channel . chanQueuedModeration <>~ cmds $ cs
  | otherwise = cs <$ traverse_ (sendMsg cs) cmds

useChanServ :: Identifier -> ConnectionState -> Bool
useChanServ channel cs =
  channel `elem` view (csSettings . ssChanservChannels) cs &&
  not (iHaveOp channel cs)
