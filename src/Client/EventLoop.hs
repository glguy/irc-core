{-# Language OverloadedStrings #-}

module Client.EventLoop
  ( eventLoop
  ) where

import           Client.ChannelState
import           Client.Commands
import           Client.ConnectionState
import           Client.Event
import           Client.Image
import           Client.Message
import           Client.State
import           Client.WordCompletion
import           Control.Concurrent
import           Control.Lens
import           Data.Time
import           Data.Foldable
import           Data.Maybe
import           Graphics.Vty
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import qualified Client.EditBox     as Edit
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap

eventLoop :: ClientState -> IO ()
eventLoop st0 =
  do st <- clientTick st0
     let vty = view clientVty st
         inQueue = view clientEvents st
         more f = eventLoop (f st)

     update vty (clientPicture st)

     event <- readChan inQueue
     case event of
       VtyEvent (EvKey k modifier) -> doKey k modifier st

       NetworkLine network time line
         | Just cs <- preview (clientConnections . ix network) st ->
         case parseRawIrcMsg (asUtf8 line) of
           Nothing ->
             do let msg = ClientMessage
                           { _msgTime = time
                           , _msgNetwork = network
                           , _msgBody = ErrorBody ("Malformed message: " ++ show line)
                           }
                more $ recordNetworkMessage msg

           Just raw ->
             do let irc = cookIrcMsg raw
                    time' = case view msgServerTime raw of
                              Nothing -> time
                              Just stime -> utcToZonedTime (zonedTimeZone time) stime
                    msg = ClientMessage
                            { _msgTime = time'
                            , _msgNetwork = network
                            , _msgBody = IrcBody irc
                            }
                    myNick = view csNick cs
                    target = msgTarget myNick irc

                -- record messages *before* applying the changes
                let (msgs, st')
                       = traverseOf
                           (clientConnections . ix network)
                           (applyMessage time irc)
                       $ recordIrcMessage network target msg
                       $ st

                traverse_ (sendMsg (view csSocket cs)) msgs
                eventLoop st'

       NetworkError network time ex ->
                do let msg = ClientMessage
                               { _msgTime = time
                               , _msgNetwork = network
                               , _msgBody = ErrorBody (show ex)
                               }
                   more $ recordNetworkMessage msg
                        . set (clientConnections . at (view msgNetwork msg)) Nothing

       NetworkClose network time ->
          do let msg = ClientMessage
                         { _msgTime = time
                         , _msgNetwork = network
                         , _msgBody = ExitBody
                         }
             more $ recordNetworkMessage msg
                  . set (clientConnections . at (view msgNetwork msg)) Nothing

       VtyEvent (EvResize w h) -> more $ set clientWidth w
                                       . set clientHeight h
       _ -> eventLoop st


doKey :: Key -> [Modifier] -> ClientState -> IO ()
doKey key modifier st =
  let changeInput f = eventLoop (over clientTextBox f st) in
  case (key,modifier) of
    (KBS      , []     ) -> changeInput Edit.backspace
    (KChar 'd', [MCtrl]) -> changeInput Edit.delete
    (KDel     , []     ) -> changeInput Edit.delete
    (KLeft    , []     ) -> changeInput Edit.left
    (KRight   , []     ) -> changeInput Edit.right
    (KHome    , []     ) -> changeInput Edit.home
    (KEnd     , []     ) -> changeInput Edit.end
    (KChar 'a', [MCtrl]) -> changeInput Edit.home
    (KChar 'e', [MCtrl]) -> changeInput Edit.end
    (KChar 'u', [MCtrl]) -> changeInput Edit.killHome
    (KChar 'k', [MCtrl]) -> changeInput Edit.killEnd
    (KChar 'y', [MCtrl]) -> changeInput Edit.paste
    (KChar 'w', [MCtrl]) -> changeInput $ Edit.killWord True
    (KChar 'b', [MMeta]) -> changeInput Edit.leftWord
    (KChar 'f', [MMeta]) -> changeInput Edit.rightWord
    (KChar 'b', [MCtrl]) -> changeInput $ Edit.insert '\^B'
    (KChar 'c', [MCtrl]) -> changeInput $ Edit.insert '\^C'
    (KChar ']', [MCtrl]) -> changeInput $ Edit.insert '\^]'
    (KChar '_', [MCtrl]) -> changeInput $ Edit.insert '\^_'
    (KChar 'o', [MCtrl]) -> changeInput $ Edit.insert '\^O'
    (KChar 'v', [MCtrl]) -> changeInput $ Edit.insert '\^V'
    (KChar 'p', [MCtrl]) -> eventLoop $ retreatFocus st
    (KChar 'n', [MCtrl]) -> eventLoop $ advanceFocus st
    (KUp      , _      ) -> changeInput $ \ed -> maybe ed id $ Edit.earlier ed
    (KDown    , _      ) -> changeInput $ \ed -> maybe ed id $ Edit.later ed
    (KChar '\t', []    ) -> tabCompletion False st
    (KBackTab  , []    ) -> tabCompletion True  st
    (KEnter   , []     ) -> execute st
    (KChar c  , []     ) -> changeInput $ Edit.insert c
    _                    -> eventLoop st

nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> ClientState
nickTabCompletion isReversed st
  = fromMaybe st
  $ clientTextBox (wordComplete isReversed completions) st
  where
    completions = completionList st

completionList :: ClientState -> [Identifier]
completionList st =
  case view clientFocus st of
    ChannelFocus network chan ->
      views (clientConnections . ix network . csChannels . ix chan . chanUsers) HashMap.keys st
    _ -> []

tabCompletion :: Bool {- ^ reversed -} -> ClientState -> IO ()
tabCompletion isReversed st =
  case clientInput st of
    '/':command -> do res <- executeCommand (Just isReversed) command st
                      case res of
                        CommandQuit -> return ()
                        CommandContinue st' -> eventLoop st'
    _          -> eventLoop (nickTabCompletion isReversed st)

execute :: ClientState -> IO ()
execute st =
  case clientInput st of
    []          -> eventLoop st
    '/':command -> do res <- executeCommand Nothing command st
                      case res of
                        CommandQuit -> return ()
                        CommandContinue st' -> eventLoop st'
    msg         -> executeChat msg st

executeChat :: String -> ClientState -> IO ()
executeChat msg st =
  case view clientFocus st of
    ChannelFocus network channel
      | Just conn <- view (clientConnections . at network) st ->
          do now <- getZonedTime
             let msgTxt = Text.pack msg
                 ircMsg = rawIrcMsg "PRIVMSG" [idText channel, msgTxt]
                 myNick = view csNick conn
                 entry = ClientMessage
                            { _msgTime = now
                            , _msgNetwork = network
                            , _msgBody = IrcBody (Privmsg myNick channel msgTxt)
                            }
             sendMsg (view csSocket conn) ircMsg
             eventLoop $ recordChannelMessage network channel entry
                       $ consumeInput st

    _ -> eventLoop st
