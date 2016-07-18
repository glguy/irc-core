{-# Language OverloadedStrings #-}

module Client.EventLoop
  ( eventLoop
  ) where

import           Client.Commands
import           Client.ConnectionState
import           Client.Image
import           Client.Message
import           Client.NetworkConnection
import           Client.State
import           Client.WordCompletion
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Data.List
import           Data.Time
import           Data.Foldable
import           Data.Maybe
import           Data.ByteString (ByteString)
import           Graphics.Vty
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import qualified Client.EditBox     as Edit
import qualified Data.Text as Text
import qualified Data.Map as Map


data ClientEvent
  = VtyEvent Event
  | NetworkEvent NetworkEvent


getEvent :: ClientState -> IO ClientEvent
getEvent st = atomically $
  asum [ VtyEvent     <$> readTChan vtyEventChannel
       , NetworkEvent <$> readTChan (view clientEvents st)
       ]
  where
    vtyEventChannel = _eventChannel (inputIface (view clientVty st))


eventLoop :: ClientState -> IO ()
eventLoop st0 =
  do st1 <- clientTick st0
     let vty = view clientVty st
         (pic, st) = clientPicture st1

     update vty pic

     event <- getEvent st
     case event of
       VtyEvent vtyEvent             -> doVtyEvent vtyEvent st
       NetworkEvent networkEvent ->
         case networkEvent of
           NetworkLine network time line -> doNetworkLine network time line st
           NetworkError network time ex  -> doNetworkError network time ex st
           NetworkClose network time     -> doNetworkClose network time st


doNetworkClose :: NetworkName -> ZonedTime -> ClientState -> IO ()
doNetworkClose network time st =
  do let msg = ClientMessage
                 { _msgTime = time
                 , _msgNetwork = network
                 , _msgBody = ExitBody
                 }
     eventLoop $ recordNetworkMessage msg
               $ set (clientConnections . at (view msgNetwork msg)) Nothing st


doNetworkError :: NetworkName -> ZonedTime -> SomeException -> ClientState -> IO ()
doNetworkError network time ex st =
  do let msg = ClientMessage
                 { _msgTime = time
                 , _msgNetwork = network
                 , _msgBody = ErrorBody (show ex)
                 }
     eventLoop $ recordNetworkMessage msg
               $ set (clientConnections . at (view msgNetwork msg)) Nothing st

doNetworkLine :: NetworkName -> ZonedTime -> ByteString -> ClientState -> IO ()
doNetworkLine network time line st =
  case preview (clientConnections . ix network) st of
    Nothing -> eventLoop st -- really shouldn't happen
    Just cs ->
      case parseRawIrcMsg (asUtf8 line) of
        Nothing ->
          do let msg = ClientMessage
                        { _msgTime = time
                        , _msgNetwork = network
                        , _msgBody = ErrorBody ("Malformed message: " ++ show line)
                        }
             eventLoop (recordNetworkMessage msg st)

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

doVtyEvent :: Event -> ClientState -> IO ()
doVtyEvent vtyEvent st =
  case vtyEvent of
    EvKey k modifier -> doKey k modifier st
    EvResize{} -> -- ignore event parameters due to raw TChan use
      do let vty = view clientVty st
         refresh vty
         (w,h) <- displayBounds (outputIface vty)
         eventLoop $ set clientWidth w
                   $ set clientHeight h st
    _                -> eventLoop st

doKey :: Key -> [Modifier] -> ClientState -> IO ()
doKey key modifier st =
  let changeInput f = eventLoop (over clientTextBox f st) in
  case modifier of
    [MCtrl] ->
      case key of
        KChar 'd' -> changeInput Edit.delete
        KChar 'a' -> changeInput Edit.home
        KChar 'e' -> changeInput Edit.end
        KChar 'u' -> changeInput Edit.killHome
        KChar 'k' -> changeInput Edit.killEnd
        KChar 'y' -> changeInput Edit.paste
        KChar 'w' -> changeInput (Edit.killWord True)
        KChar 'b' -> changeInput (Edit.insert '\^B')
        KChar 'c' -> changeInput (Edit.insert '\^C')
        KChar ']' -> changeInput (Edit.insert '\^]')
        KChar '_' -> changeInput (Edit.insert '\^_')
        KChar 'o' -> changeInput (Edit.insert '\^O')
        KChar 'v' -> changeInput (Edit.insert '\^V')
        KChar 'p' -> eventLoop (retreatFocus st)
        KChar 'n' -> eventLoop (advanceFocus st)
        KChar 'l' -> refreshClient st
        _         -> eventLoop st

    [MMeta] ->
      case key of
        KChar 'b' -> changeInput Edit.leftWord
        KChar 'f' -> changeInput Edit.rightWord
        KChar c   | Just i <- elemIndex c windowNames ->
                            eventLoop (jumpFocus i st)
        _ -> eventLoop st

    [] -> -- no modifier
      case key of
        KBS        -> changeInput Edit.backspace
        KDel       -> changeInput Edit.delete
        KLeft      -> changeInput Edit.left
        KRight     -> changeInput Edit.right
        KHome      -> changeInput Edit.home
        KEnd       -> changeInput Edit.end
        KUp        -> changeInput $ \ed -> maybe ed id $ Edit.earlier ed
        KDown      -> changeInput $ \ed -> maybe ed id $ Edit.later ed
        KEnter     -> execute st
        KPageUp    -> eventLoop (pageUp st)
        KPageDown  -> eventLoop (pageDown st)
        KBackTab   -> tabCompletion True  st
        KChar '\t' -> tabCompletion False st
        KChar c    -> changeInput (Edit.insert c)
        _          -> eventLoop st

    _ -> eventLoop st -- unsupported modifier

refreshClient :: ClientState -> IO ()
refreshClient st =
  do refresh (view clientVty st)
     eventLoop st

pageUp :: ClientState -> ClientState
pageUp st = over clientScroll (+ scrollAmount st) st

pageDown :: ClientState -> ClientState
pageDown st = over clientScroll (max 0 . subtract (scrollAmount st)) st

scrollAmount :: ClientState -> Int
scrollAmount st = max 1 (view clientHeight st - 2)

jumpFocus :: Int -> ClientState -> ClientState
jumpFocus i st
  | 0 <= i, i < Map.size windows = changeFocus focus st
  | otherwise                    = st
  where
    windows = view clientWindows st
    (focus,_) = Map.elemAt i windows

nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> ClientState
nickTabCompletion isReversed st
  = fromMaybe st
  $ clientTextBox (wordComplete isReversed completions) st
  where
    completions = currentUserList st

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
                 myNick = UserInfo (view csNick conn) Nothing Nothing
                 entry = ClientMessage
                            { _msgTime = now
                            , _msgNetwork = network
                            , _msgBody = IrcBody (Privmsg myNick channel msgTxt)
                            }
             sendMsg (view csSocket conn) ircMsg
             eventLoop $ recordChannelMessage network channel entry
                       $ consumeInput st

    _ -> eventLoop st
