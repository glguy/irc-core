{-# Language OverloadedStrings #-}

{-|
Module      : Client.EventLoop
Description : Event loop for IRC client
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module is responsible for dispatching user-input, network, and timer
events to the correct module. It renders the user interface once per event.
-}

module Client.EventLoop
  ( eventLoop
  ) where

import           Client.Commands
import           Client.ConnectionState
import qualified Client.EditBox     as Edit
import           Client.Hook
import           Client.Hooks
import           Client.Image
import           Client.Message
import           Client.NetworkConnection
import           Client.State
import           Client.Window
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Foldable
import qualified Data.IntMap as IntMap
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as Text
import           Data.Time
import           Graphics.Vty
import           Irc.Message
import           Irc.RawIrcMsg


-- | Sum of the three possible event types the event loop handles
data ClientEvent
  = VtyEvent Event -- ^ Key presses and resizing
  | NetworkEvent NetworkEvent -- ^ Incoming network events
  | TimerEvent NetworkId TimedAction -- ^ Timed action and the applicable network


-- | Block waiting for the next 'ClientEvent'. This function will compute
-- an appropriate timeout based on the current connections.
getEvent :: ClientState -> IO ClientEvent
getEvent st =
  do timer <- prepareTimer
     atomically $
       asum [ timer
            , VtyEvent     <$> readTChan vtyEventChannel
            , NetworkEvent <$> readTQueue (view clientEvents st)
            ]
  where
    vtyEventChannel = _eventChannel (inputIface (view clientVty st))

    possibleTimedEvents =
      [ (networkId, runAt, action)
           | (networkId, cs) <- views clientConnections IntMap.toList st
           , Just (runAt, action) <- [nextTimedAction cs]
           ]

    earliestEvent
      | null possibleTimedEvents = Nothing
      | otherwise = Just (minimumBy (comparing (\(_,runAt,_) -> runAt)) possibleTimedEvents)

    prepareTimer =
      case earliestEvent of
        Nothing -> return retry
        Just (networkId,runAt,action) ->
          do now <- getCurrentTime
             let microsecs = truncate (1000000 * diffUTCTime runAt now)
             var <- registerDelay (max 0 microsecs)
             return $ do ready <- readTVar var
                         unless ready retry
                         return (TimerEvent networkId action)

-- | Apply this function to an initial 'ClientState' to launch the client.
eventLoop :: ClientState -> IO ()
eventLoop st0 =
  do let st1 = clientTick st0
         vty = view clientVty st
         (pic, st) = clientPicture st1

     -- check st0 for bell, it will be always be cleared in st1
     when (view clientBell st0) (beep vty)
     update vty pic

     event <- getEvent st
     case event of
       TimerEvent networkId action  -> doTimerEvent networkId action st
       VtyEvent vtyEvent         -> doVtyEvent vtyEvent st
       NetworkEvent networkEvent ->
         case networkEvent of
           NetworkLine  network time line -> doNetworkLine network time line st
           NetworkError network time ex   -> doNetworkError network time ex st
           NetworkClose network time      -> doNetworkClose network time st

-- | Sound the terminal bell assuming that the @BEL@ control code
-- is supported.
beep :: Vty -> IO ()
beep vty = outputByteBuffer (outputIface vty) "\BEL"

-- | Respond to a network connection closing normally.
doNetworkClose ::
  NetworkId {- ^ finished network -} ->
  ZonedTime {- ^ current time     -} ->
  ClientState -> IO ()
doNetworkClose networkId time st =
  let (cs,st') = removeNetwork networkId st
      msg = ClientMessage
              { _msgTime    = time
              , _msgNetwork = view csNetwork cs
              , _msgBody    = ExitBody
              }
  in eventLoop $ recordNetworkMessage msg st'


-- | Respond to a network connection closing abnormally.
doNetworkError ::
  NetworkId {- ^ failed network -} ->
  ZonedTime {- ^ current time   -} ->
  SomeException {- ^ termination reason -} ->
  ClientState -> IO ()
doNetworkError networkId time ex st =
  let (cs,st') = removeNetwork networkId st
      msg = ClientMessage
              { _msgTime    = time
              , _msgNetwork = view csNetwork cs
              , _msgBody    = ErrorBody (show ex)
              }
  in eventLoop $ recordNetworkMessage msg st'


-- | Respond to an IRC protocol line. This will parse the message, updated the
-- relevant connection state and update the UI buffers.
doNetworkLine ::
  NetworkId {- ^ Network ID of message -} ->
  ZonedTime {- ^ current time          -} ->
  ByteString {- ^ Raw IRC message without newlines -} ->
  ClientState -> IO ()
doNetworkLine networkId time line st =
  case view (clientConnections . at networkId) st of
    Nothing -> error "doNetworkLine: Network missing"
    Just cs ->
      let network = view csNetwork cs in
      case parseRawIrcMsg (asUtf8 line) of
        Nothing ->
          do let msg = ClientMessage
                        { _msgTime = time
                        , _msgNetwork = network
                        , _msgBody = ErrorBody ("Malformed message: " ++ show line)
                        }
             eventLoop (recordNetworkMessage msg st)

        Just raw ->
          do let time' = computeEffectiveTime time (view msgTags raw)

                 (stateHook, viewHook)
                      = over both applyMessageHooks
                      $ partition (view messageHookStateful)
                      $ lookups
                          (view csMessageHooks cs)
                          messageHooks

             case stateHook (cookIrcMsg raw) of
               Nothing  -> eventLoop st -- Message ignored
               Just irc -> do traverse_ (sendMsg cs) replies
                              eventLoop st'
                 where
                   -- state with message recorded
                   recSt = case viewHook irc of
                             Nothing   -> st -- Message hidden
                             Just irc' -> recordIrcMessage network target msg st
                               where
                                 myNick = view csNick cs
                                 target = msgTarget myNick irc
                                 msg = ClientMessage
                                         { _msgTime    = time'
                                         , _msgNetwork = network
                                         , _msgBody    = IrcBody irc'
                                         }

                   -- record messages *before* applying the changes
                   (replies, st') = applyMessageToClientState time irc networkId cs recSt

-- | Find the ZNC provided server time
computeEffectiveTime :: ZonedTime -> [TagEntry] -> ZonedTime
computeEffectiveTime time tags = fromMaybe time zncTime
  where
    isTimeTag (TagEntry key _) = key == "time"
    zncTime =
      do TagEntry _ txt <- find isTimeTag tags
         tagTime <- parseZncTime (Text.unpack txt)
         return (utcToZonedTime (zonedTimeZone time) tagTime)

-- | Parses the time format used by ZNC for buffer playback
parseZncTime :: String -> Maybe UTCTime
parseZncTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z"


-- | Returns the list of values that were stored at the given indexes, if
-- a value was stored at that index.
lookups :: Ixed m => [Index m] -> m -> [IxValue m]
lookups ks m = mapMaybe (\k -> preview (ix k) m) ks


-- | Respond to a VTY event.
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
    _ -> eventLoop st


-- | Map keyboard inputs to actions in the client
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
        KChar 'l' -> refresh (view clientVty st) >> eventLoop st
        _         -> eventLoop st

    [MMeta] ->
      case key of
        KBS       -> changeInput (Edit.killWord True)
        KChar 'b' -> changeInput Edit.leftWord
        KChar 'f' -> changeInput Edit.rightWord
        KChar 'a' -> eventLoop (jumpToActivity st)
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
        KUp        -> changeInput $ \ed -> fromMaybe ed $ Edit.earlier ed
        KDown      -> changeInput $ \ed -> fromMaybe ed $ Edit.later ed
        KPageUp    -> eventLoop (pageUp st)
        KPageDown  -> eventLoop (pageDown st)

        KEnter     -> doCommandResult =<< execute st
        KBackTab   -> doCommandResult =<< tabCompletion True  st
        KChar '\t' -> doCommandResult =<< tabCompletion False st

        KChar c    -> changeInput (Edit.insert c)
        KFun 2     -> eventLoop (over clientDetailView not st)
        _          -> eventLoop st

    _ -> eventLoop st -- unsupported modifier

-- | Process 'CommandResult' by either running the 'eventLoop' with the
-- new 'ClientState' or returning.
doCommandResult :: CommandResult -> IO ()
doCommandResult res =
  case res of
    CommandQuit        -> return ()
    CommandContinue st -> eventLoop st

-- | Scroll the current buffer to show older messages
pageUp :: ClientState -> ClientState
pageUp st = over clientScroll (+ scrollAmount st) st

-- | Scroll the current buffer to show newer messages
pageDown :: ClientState -> ClientState
pageDown st = over clientScroll (max 0 . subtract (scrollAmount st)) st

-- | Compute the number of lines in a page at the current window size
scrollAmount :: ClientState -> Int
scrollAmount st = max 1 (view clientHeight st - 2)

-- | Jump the focus of the client to a buffer that has unread activity.
-- Some events like errors or chat messages mentioning keywords are
-- considered important and will be jumped to first.
jumpToActivity :: ClientState -> ClientState
jumpToActivity st =
  case mplus highPriority lowPriority of
    Just (focus,_) -> changeFocus focus st
    Nothing        -> st
  where
    windowList = views clientWindows Map.toList st
    highPriority = find (view winMention . snd) windowList
    lowPriority  = find (\x -> view winUnread (snd x) > 0) windowList

-- | Jump the focus directly to a window based on its zero-based index.
jumpFocus ::
  Int {- ^ zero-based window index -} ->
  ClientState -> ClientState
jumpFocus i st
  | 0 <= i, i < Map.size windows = changeFocus focus st
  | otherwise                    = st
  where
    windows = view clientWindows st
    (focus,_) = Map.elemAt i windows


-- | Respond to a timer event.
doTimerEvent ::
  NetworkId {- ^ Network related to event -} ->
  TimedAction {- ^ Action to perform -} ->
  ClientState -> IO ()
doTimerEvent networkId action =
  eventLoop <=< traverseOf (clientConnections . ix networkId)
                           (applyTimedAction action)
