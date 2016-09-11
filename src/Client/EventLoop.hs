{-# Language OverloadedStrings, NondecreasingIndentation #-}

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

import           Client.CApi
import           Client.Commands
import           Client.Commands.Interpolation
import           Client.Configuration.ServerSettings
import           Client.EventLoop.Errors (exceptionToLines)
import           Client.Hook
import           Client.Hooks
import           Client.Image
import           Client.Message
import           Client.Network.Async
import           Client.State
import qualified Client.State.EditBox     as Edit
import           Client.State.Focus
import           Client.State.Network
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import           Data.Time
import           GHC.IO.Exception (IOErrorType(..), ioe_type)
import           Graphics.Vty
import           Irc.Codes
import           Irc.Message
import           Irc.RawIrcMsg
import           LensUtils
import           Network.Connection

reconnectAttempts :: Int
reconnectAttempts = 6

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

    prepareTimer =
      case earliestEvent st of
        Nothing -> return retry
        Just (networkId,(runAt,action)) ->
          do now <- getCurrentTime
             let microsecs = truncate (1000000 * diffUTCTime runAt now)
             var <- registerDelay (max 0 microsecs)
             return $ do ready <- readTVar var
                         unless ready retry
                         return (TimerEvent networkId action)

-- | Compute the earliest scheduled timed action for the client
earliestEvent :: ClientState -> Maybe (NetworkId, (UTCTime, TimedAction))
earliestEvent =
  minimumByOf
    (clientConnections . (ifolded <. folding nextTimedAction) . withIndex)
    (comparing (fst . snd))

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
           NetworkOpen  network time      -> doNetworkOpen  network time st
           NetworkClose network time      -> doNetworkClose network time st

-- | Sound the terminal bell assuming that the @BEL@ control code
-- is supported.
beep :: Vty -> IO ()
beep = ringTerminalBell . outputIface

-- | Respond to a network connection successfully connecting.
doNetworkOpen ::
  NetworkId   {- ^ network id   -} ->
  ZonedTime   {- ^ event time   -} ->
  ClientState {- ^ client state -} ->
  IO ()
doNetworkOpen networkId time st =
  case view (clientConnections . at networkId) st of
    Nothing -> error "doNetworkOpen: Network missing"
    Just cs ->
      let msg = ClientMessage
                  { _msgTime    = time
                  , _msgNetwork = view csNetwork cs
                  , _msgBody    = NormalBody "connection opened"
                  }
      in eventLoop $ recordNetworkMessage msg
                   $ overStrict (clientConnections . ix networkId . csLastReceived)
                                (\old -> old `seq` Just $! zonedTimeToUTC time) st

-- | Respond to a network connection closing normally.
doNetworkClose ::
  NetworkId {- ^ network id -} ->
  ZonedTime {- ^ event time -} ->
  ClientState -> IO ()
doNetworkClose networkId time st =
  let (cs,st') = removeNetwork networkId st
      msg = ClientMessage
              { _msgTime    = time
              , _msgNetwork = view csNetwork cs
              , _msgBody    = NormalBody "connection closed"
              }
  in eventLoop $ recordNetworkMessage msg st'


-- | Respond to a network connection closing abnormally.
doNetworkError ::
  NetworkId {- ^ failed network -} ->
  ZonedTime {- ^ current time   -} ->
  SomeException {- ^ termination reason -} ->
  ClientState -> IO ()
doNetworkError networkId time ex st =
  do let (cs,st1) = removeNetwork networkId st
         st2 = foldl' (\acc msg -> recordError time cs (Text.pack msg) acc) st1
             $ exceptionToLines ex

         shouldReconnect =
           case view csPingStatus cs of
             PingConnecting n _
               | n == 0 || n > reconnectAttempts -> False

             _ | Just HostNotResolved{}   <-              fromException ex -> True
               | Just HostCannotConnect{} <-              fromException ex -> True
               | Just PingTimeout         <-              fromException ex -> True
               | Just ResourceVanished    <- ioe_type <$> fromException ex -> True
               | Just NoSuchThing         <- ioe_type <$> fromException ex -> True

               | otherwise -> False


         reconnect = do
           (attempts, mbDisconnectTime)
              <- case view csPingStatus cs of
                   PingConnecting n tm                   -> pure (n+1, tm)
                   _ | Just tm <- view csLastReceived cs -> pure (1, Just tm)
                     | otherwise -> do now <- getCurrentTime
                                       pure (1, Just now)
           addConnection attempts mbDisconnectTime (view csNetwork cs) st2

         nextAction
           | shouldReconnect = reconnect
           | otherwise       = return st2

     eventLoop =<< nextAction


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
          do let msg = Text.pack ("Malformed message: " ++ show line)
             eventLoop (recordError time cs msg st)

        Just raw ->
          do (st1,passed) <- clientPark st $ \ptr ->
                               notifyExtensions ptr network raw
                                 (view (clientExtensions . esActive) st)


             if not passed then eventLoop st1 else do

             let time' = computeEffectiveTime time (view msgTags raw)

                 (stateHook, viewHook)
                      = over both applyMessageHooks
                      $ partition (view messageHookStateful)
                      $ lookups
                          (view csMessageHooks cs)
                          messageHooks

             case stateHook (cookIrcMsg raw) of
               Nothing  -> eventLoop st1 -- Message ignored
               Just irc -> do traverse_ (sendMsg cs) replies
                              st3 <- clientResponse time' irc cs st2
                              eventLoop st3
                 where
                   -- state with message recorded
                   recSt = case viewHook irc of
                             Nothing   -> st1 -- Message hidden
                             Just irc' -> recordIrcMessage network target msg st1
                               where
                                 myNick = view csNick cs
                                 target = msgTarget myNick irc
                                 msg = ClientMessage
                                         { _msgTime    = time'
                                         , _msgNetwork = network
                                         , _msgBody    = IrcBody irc'
                                         }

                   -- record messages *before* applying the changes
                   --
                   -- Note: it's important to do this before 'clientResponse'
                   -- as $nick won't be set until 'doWelcome' happens.
                   (replies, st2) = applyMessageToClientState time irc networkId cs recSt

-- | Client-level responses to specific IRC messages.
-- This is in contrast to the connection state tracking logic in
-- "Client.NetworkState   "
clientResponse :: ZonedTime -> IrcMsg -> NetworkState -> ClientState -> IO ClientState
clientResponse now irc cs st =
  case irc of
    Reply RPL_WELCOME _ ->
      foldM
        (processConnectCmd now cs)
        st
        (view (csSettings . ssConnectCmds) cs)
    _ -> return st

processConnectCmd ::
  ZonedTime       {- ^ now             -} ->
  NetworkState    {- ^ current network -} ->
  ClientState     {- ^ client state    -} ->
  [ExpansionChunk]{- ^ command         -} ->
  IO ClientState
processConnectCmd now cs st0 cmdTxt =
  do dc <- forM disco $ \t ->
             Text.pack . formatTime defaultTimeLocale "%H:%M:%S"
               <$> utcToLocalZonedTime t
     let failureCase e = recordError now cs ("Bad connect-cmd: " <> e)
     case resolveMacroExpansions (commandExpansion dc st0) (const Nothing) cmdTxt of
       Nothing -> return $! failureCase "Unable to expand connect command" st0
       Just cmdTxt' ->
         do res <- executeUserCommand dc (Text.unpack cmdTxt') st0
            return $! case res of
              CommandFailure st -> failureCase cmdTxt' st
              CommandSuccess st -> st
              CommandQuit    st -> st -- not supported
 where
 disco = case view csPingStatus cs of
   PingConnecting _ tm -> tm
   _ -> Nothing


recordError ::
  ZonedTime       {- ^ now             -} ->
  NetworkState    {- ^ current network -} ->
  Text            {- ^ error message   -} ->
  ClientState     {- ^ client state    -} ->
  ClientState
recordError now cs msg =
  recordNetworkMessage ClientMessage
    { _msgTime    = now
    , _msgNetwork = view csNetwork cs
    , _msgBody    = ErrorBody msg
    }

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
    EvPaste utf8 ->
       let str = Text.unpack (Text.decodeUtf8With Text.lenientDecode utf8)
           st' = over clientTextBox (Edit.insertPaste str) st
       in eventLoop st'
    _ -> eventLoop st


-- | Map keyboard inputs to actions in the client
doKey :: Key -> [Modifier] -> ClientState -> IO ()
doKey key modifier st =
  let changeEditor  f = eventLoop (over clientTextBox f st)
      changeContent f = changeEditor
                      $ over Edit.content f
                      . set  Edit.lastOperation Edit.OtherOperation
  in
  case modifier of
    [MCtrl] ->
      case key of
        KChar 'd' -> changeContent Edit.delete
        KChar 'a' -> changeEditor Edit.home
        KChar 'e' -> changeEditor Edit.end
        KChar 'u' -> changeEditor Edit.killHome
        KChar 'k' -> changeEditor Edit.killEnd
        KChar 'y' -> changeEditor Edit.yank
        KChar 't' -> changeContent Edit.toggle
        KChar 'w' -> changeEditor (Edit.killWordBackward True)
        KChar 'b' -> changeEditor (Edit.insert '\^B')
        KChar 'c' -> changeEditor (Edit.insert '\^C')
        KChar ']' -> changeEditor (Edit.insert '\^]')
        KChar '_' -> changeEditor (Edit.insert '\^_')
        KChar 'o' -> changeEditor (Edit.insert '\^O')
        KChar 'v' -> changeEditor (Edit.insert '\^V')
        KChar 'p' -> eventLoop (retreatFocus st)
        KChar 'n' -> eventLoop (advanceFocus st)
        KChar 'l' -> refresh (view clientVty st) >> eventLoop st
        _         -> eventLoop st

    [MMeta] ->
      case key of
        KEnter    -> changeEditor (Edit.insert '\^J')
        KBS       -> changeEditor (Edit.killWordBackward True)
        KChar 'd' -> changeEditor (Edit.killWordForward True)
        KChar 'b' -> changeContent Edit.leftWord
        KChar 'f' -> changeContent Edit.rightWord
        KChar 'a' -> eventLoop (jumpToActivity st)
        KChar 's' -> eventLoop (returnFocus st)
        KChar c   | let names = clientWindowNames st
                  , Just i <- elemIndex c names ->
                            eventLoop (jumpFocus i st)
        _ -> eventLoop st

    [] -> -- no modifier
      case key of
        KEsc       -> eventLoop (changeSubfocus FocusMessages st)
        KBS        -> changeContent Edit.backspace
        KDel       -> changeContent Edit.delete
        KLeft      -> changeContent Edit.left
        KRight     -> changeContent Edit.right
        KHome      -> changeEditor Edit.home
        KEnd       -> changeEditor Edit.end
        KUp        -> changeEditor $ \ed -> fromMaybe ed $ Edit.earlier ed
        KDown      -> changeEditor $ \ed -> fromMaybe ed $ Edit.later ed
        KPageUp    -> eventLoop (pageUp st)
        KPageDown  -> eventLoop (pageDown st)

        KEnter     -> doCommandResult True  =<< executeInput st
        KBackTab   -> doCommandResult False =<< tabCompletion True  st
        KChar '\t' -> doCommandResult False =<< tabCompletion False st

        KChar c    -> changeEditor (Edit.insert c)

        -- toggles
        KFun 2     -> eventLoop (over clientDetailView  not st)
        KFun 3     -> eventLoop (over clientActivityBar not st)
        KFun 4     -> eventLoop (over clientShowMetadata not st)

        _          -> eventLoop st

    _ -> eventLoop st -- unsupported modifier

-- | Process 'CommandResult' by either running the 'eventLoop' with the
-- new 'ClientState' or returning.
doCommandResult :: Bool -> CommandResult -> IO ()
doCommandResult clearOnSuccess res =
  case res of
    CommandQuit    st -> clientShutdown st
    CommandSuccess st -> eventLoop (if clearOnSuccess then consumeInput st else st)
    CommandFailure st -> eventLoop (set clientBell True st)

executeInput :: ClientState -> IO CommandResult
executeInput st = execute (clientFirstLine st) st


-- | Respond to a timer event.
doTimerEvent ::
  NetworkId {- ^ Network related to event -} ->
  TimedAction {- ^ Action to perform -} ->
  ClientState -> IO ()
doTimerEvent networkId action =
  eventLoop <=< traverseOf (clientConnections . ix networkId)
                           (applyTimedAction action)
