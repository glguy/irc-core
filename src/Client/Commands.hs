{-# LANGUAGE OverloadedStrings #-}

module Client.Commands
  ( CommandResult(..)
  , executeCommand
  ) where

import           Client.Configuration
import           Client.ConnectionState
import           Client.Message
import           Client.NetworkConnection
import           Client.ServerSettings
import           Client.ChannelState
import           Client.State
import           Control.Lens
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Irc.Identifier
import           Irc.RawIrcMsg
import           Irc.Message
import qualified Client.EditBox as Edit

data CommandResult
  = CommandContinue ClientState
  | CommandQuit

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

addConnection :: String -> ClientState -> IO ClientState
addConnection host st =
  do let network = Text.pack host
         defSettings = (view (clientConfig . configDefaults) st)
                     { _ssHostName = host }
         settings = fromMaybe defSettings
                              (view (clientConfig . configServers . at host) st)

     c <- createConnection
            network
            (view clientConnectionContext st)
            settings
            (view clientEvents st)

     let cs = newConnectionState settings c
     sendMsg c capLsMsg

     return $! set (clientConnections . at network) (Just cs) st


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
executeCommand tabCompleteReversed str st =
  let (cmd, rest) = splitWord str in
  case HashMap.lookup (Text.toLower (Text.pack cmd)) commands of

    Just (ClientCommand exec tab) ->
          maybe exec tab tabCompleteReversed
            st rest

    Just (NetworkCommand exec tab)
      | Just network <- views clientFocus focusNetwork st
      , Just cs      <- preview (clientConnections . ix network) st ->
          maybe exec tab tabCompleteReversed
            network cs st rest

    Just (ChannelCommand exec tab)
      | ChannelFocus network channelId <- view clientFocus st
      , Just cs <- preview (clientConnections . ix network) st
      , isChannelIdentifier cs channelId ->
          maybe exec tab tabCompleteReversed
            network cs channelId st rest

    _ -> commandContinue st

commands :: HashMap Text Command
commands = HashMap.fromList
  [ ("connect", ClientCommand cmdConnect noClientTab)
  , ("exit"   , ClientCommand cmdExit    noClientTab)
  , ("focus"  , ClientCommand cmdFocus   noClientTab)

  , ("quote"  , NetworkCommand cmdQuote  noNetworkTab)
  , ("join"   , NetworkCommand cmdJoin   noNetworkTab)
  , ("mode"   , NetworkCommand cmdMode   noNetworkTab)
  , ("msg"    , NetworkCommand cmdMsg    noNetworkTab)
  , ("nick"   , NetworkCommand cmdNick   noNetworkTab)
  , ("quit"   , NetworkCommand cmdQuit   noNetworkTab)
  , ("whois"  , NetworkCommand cmdWhois  noNetworkTab)
  , ("whowas" , NetworkCommand cmdWhowas noNetworkTab)

  , ("invite" , ChannelCommand cmdInvite noChannelTab)
  , ("topic"  , ChannelCommand cmdTopic  tabTopic    )
  , ("kick"   , ChannelCommand cmdKick   noChannelTab)
  , ("me"     , ChannelCommand cmdMe     noChannelTab)
  , ("part"   , ChannelCommand cmdPart   noChannelTab)
  ]

noClientTab :: Bool -> ClientCommand
noClientTab _ st _ = commandContinue st

noNetworkTab :: Bool -> NetworkCommand
noNetworkTab _ _ _ st _ = commandContinue st

noChannelTab :: Bool -> ChannelCommand
noChannelTab _ _ _ _ st _ = commandContinue st

cmdExit :: ClientState -> String -> IO CommandResult
cmdExit _ _ = return CommandQuit

cmdQuote :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdQuote _ cs st rest =
  case parseQuote rest of
    Nothing  -> commandContinue st
    Just raw ->
      do sendMsg (view csSocket cs) raw
         commandContinue (consumeInput st)

parseQuote :: String -> Maybe RawIrcMsg
parseQuote str =
  case Text.pack <$> toArgList str of
    cmd:args -> Just (rawIrcMsg cmd args)
    _        -> Nothing

toArgList :: String -> [String]
toArgList xs =
  case dropWhile isSpace xs of
    ':':final -> [final]
    ""        -> []
    xs1 | (arg,xs2) <- splitWord xs1 -> arg : toArgList xs2

-- | Implementation of @/me@
cmdMe :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdMe network cs channelId st rest =
  do now <- getZonedTime
     let actionTxt = Text.pack ("\^AACTION " ++ rest ++ "\^A")
         myNick = view csNick cs
         entry = ClientMessage
                    { _msgTime = now
                    , _msgNetwork = network
                    , _msgBody = IrcBody (Action myNick channelId (Text.pack rest))
                    }
     sendMsg
       (view csSocket cs)
       (rawIrcMsg "PRIVMSG" [idText channelId, actionTxt])
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
         myNick = view csNick cs
         entries = [ (targetId,
                      ClientMessage
                      { _msgTime = now
                      , _msgNetwork = network
                      , _msgBody = IrcBody (Privmsg myNick targetId msgTxt)
                      })
                   | targetId <- targetIds ]

     for_ targetTxts $ \targetTxt ->
       sendMsg (view csSocket cs) (rawIrcMsg "PRIVMSG" [targetTxt, msgTxt])

     let st' = foldl' (\acc (targetId, entry) ->
                         recordChannelMessage network targetId entry acc)
                      st
                      entries

     commandContinue (consumeInput st')

cmdConnect :: ClientState -> String -> IO CommandResult
cmdConnect st rest =
  case words rest of
    [network] ->
      do st' <- addConnection network $ consumeInput st
         commandContinue $ set clientFocus (NetworkFocus (Text.pack network)) st'
    _ -> commandContinue st

cmdFocus :: ClientState -> String -> IO CommandResult
cmdFocus st rest =
  case words rest of
    [network] ->
      let focus = NetworkFocus (Text.pack network) in
      commandContinue
        $ set clientFocus focus
        $ consumeInput st

    [network,channel] ->
      let focus = ChannelFocus (Text.pack network) (mkId (Text.pack channel)) in
      commandContinue
        $ set clientFocus focus
        $ consumeInput st

    _ -> commandContinue st

cmdWhois :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdWhois _ cs st rest =
  do sendMsg (view csSocket cs) (rawIrcMsg "WHOIS" (Text.pack <$> words rest))
     commandContinue (consumeInput st)

cmdWhowas :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdWhowas _ cs st rest =
  do sendMsg (view csSocket cs) (rawIrcMsg "WHOWAS" (Text.pack <$> words rest))
     commandContinue (consumeInput st)

cmdMode :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdMode _ cs st rest = modeCommand (Text.pack <$> words rest) cs st

cmdNick :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdNick _ cs st rest =
  case words rest of
    [nick] ->
      do sendMsg (view csSocket cs) (rawIrcMsg "NICK" [Text.pack nick])
         commandContinue (consumeInput st)
    _ -> commandContinue st

cmdPart :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdPart _ cs channelId st rest =
  do let msgs = case dropWhile isSpace rest of
                  ""  -> []
                  msg -> [Text.pack msg]
     sendMsg (view csSocket cs) (rawIrcMsg "PART" (idText channelId : msgs))
     commandContinue (consumeInput st)

cmdInvite :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdInvite _ cs channelId st rest =
  case words rest of
    [nick] ->
      do sendMsg (view csSocket cs) (rawIrcMsg "INVITE" [Text.pack nick, idText channelId])
         commandContinue (consumeInput st)
    _ -> commandContinue st

cmdTopic :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdTopic _ cs channelId st rest =
  do let topics = case dropWhile isSpace rest of
                    ""    -> []
                    topic -> [Text.pack topic]
     sendMsg (view csSocket cs) (rawIrcMsg "TOPIC" (idText channelId : topics))
     commandContinue (consumeInput st)

tabTopic :: Bool -> NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
tabTopic _ _ cs channelId st rest

  | all isSpace rest
  , Just topic <- preview (csChannels . ix channelId . chanTopic) cs =
     do let textBox = Edit.end
                    . set Edit.content ("/topic " ++ Text.unpack topic)
        commandContinue (over clientTextBox textBox st)

  | otherwise = commandContinue st

cmdKick :: NetworkName -> ConnectionState -> Identifier -> ClientState -> String -> IO CommandResult
cmdKick _ cs channelId st rest =
  do let (who,reason) = nextWord rest
         msgs = case dropWhile isSpace reason of
                  "" -> []
                  msg -> [Text.pack msg]
     sendMsg (view csSocket cs) $ rawIrcMsg "KICK" (idText channelId : Text.pack who : msgs)
     commandContinue $ consumeInput st

cmdJoin :: NetworkName -> ConnectionState -> ClientState -> String -> IO CommandResult
cmdJoin network cs st rest =
  let ws = words rest
      doJoin channelTxt =
        do let channelId = mkId (Text.pack (takeWhile (/=',') channelTxt))
           sendMsg (view csSocket cs) $ rawIrcMsg "JOIN" (Text.pack <$> ws)
           commandContinue
               $ set clientFocus (ChannelFocus network channelId)
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
     sendMsg (view csSocket cs) (rawIrcMsg "QUIT" msgs)
     commandContinue (consumeInput st)

modeCommand :: [Text] -> ConnectionState -> ClientState -> IO CommandResult
modeCommand modes cs st =
  case view clientFocus st of

    NetworkFocus _ ->
      do sendMsg (view csSocket cs) (rawIrcMsg "MODE" (idText (view csNick cs) : modes))
         commandContinue (consumeInput st)

    ChannelFocus _ chan ->
          do sendMsg conn (rawIrcMsg "MODE" (idText chan : modes))
             commandContinue (consumeInput st)

    _ -> commandContinue st

  where
    conn = view csSocket cs
