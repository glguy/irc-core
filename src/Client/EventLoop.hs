{-# Language BangPatterns, OverloadedStrings, NondecreasingIndentation #-}

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
  , updateTerminalSize
  ) where

import qualified Client.Authentication.Ecdsa as Ecdsa
import           Client.CApi
import           Client.Commands
import           Client.Commands.Interpolation
import           Client.Configuration (configJumpModifier, configKeyMap, configWindowNames)
import           Client.Configuration.ServerSettings
import           Client.EventLoop.Actions
import           Client.EventLoop.Errors (exceptionToLines)
import           Client.Hook
import           Client.Hooks
import           Client.Image
import           Client.Image.Layout (scrollAmount)
import           Client.Log
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
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import           Data.Time
import           GHC.IO.Exception (IOErrorType(..), ioe_type)
import           Graphics.Vty
import           Irc.Codes
import           Irc.Commands
import           Irc.Message
import           Irc.RawIrcMsg
import           LensUtils
import           Hookup


-- | Sum of the three possible event types the event loop handles
data ClientEvent
  = VtyEvent Event -- ^ Key presses and resizing
  | NetworkEvent NetworkEvent -- ^ Incoming network events
  | TimerEvent NetworkId TimedAction -- ^ Timed action and the applicable network


-- | Block waiting for the next 'ClientEvent'. This function will compute
-- an appropriate timeout based on the current connections.
getEvent ::
  Vty         {- ^ vty handle   -} ->
  ClientState {- ^ client state -} ->
  IO ClientEvent
getEvent vty st =
  do timer <- prepareTimer
     atomically $
       asum [ timer
            , VtyEvent     <$> readTChan vtyEventChannel
            , NetworkEvent <$> readTQueue (view clientEvents st)
            ]
  where
    vtyEventChannel = _eventChannel (inputIface vty)

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
eventLoop :: Vty -> ClientState -> IO ()
eventLoop vty st =
  do when (view clientBell st) (beep vty)
     processLogEntries st

     let (pic, st') = clientPicture (clientTick st)
     update vty pic

     event <- getEvent vty st'
     case event of
       TimerEvent networkId action  -> eventLoop vty =<< doTimerEvent networkId action st'
       VtyEvent vtyEvent -> traverse_ (eventLoop vty) =<< doVtyEvent vty vtyEvent st'
       NetworkEvent networkEvent ->
         eventLoop vty =<<
         case networkEvent of
           NetworkLine  net time line -> doNetworkLine  net time line st'
           NetworkError net time ex   -> doNetworkError net time ex st'
           NetworkOpen  net time      -> doNetworkOpen  net time st'
           NetworkClose net time      -> doNetworkClose net time st'

-- | Sound the terminal bell assuming that the @BEL@ control code
-- is supported.
beep :: Vty -> IO ()
beep = ringTerminalBell . outputIface

processLogEntries :: ClientState -> IO ()
processLogEntries =
  traverse_ writeLogLine . reverse . view clientLogQueue

-- | Respond to a network connection successfully connecting.
doNetworkOpen ::
  NetworkId   {- ^ network id   -} ->
  ZonedTime   {- ^ event time   -} ->
  ClientState {- ^ client state -} ->
  IO ClientState
doNetworkOpen networkId time st =
  case view (clientConnections . at networkId) st of
    Nothing -> error "doNetworkOpen: Network missing"
    Just cs ->
      do let msg = ClientMessage
                     { _msgTime    = time
                     , _msgNetwork = view csNetwork cs
                     , _msgBody    = NormalBody "connection opened"
                     }
         return $! recordNetworkMessage msg
                 $ overStrict (clientConnections . ix networkId . csLastReceived)
                              (\old -> old `seq` Just $! zonedTimeToUTC time)
                              st

-- | Respond to a network connection closing normally.
doNetworkClose ::
  NetworkId   {- ^ network id   -} ->
  ZonedTime   {- ^ event time   -} ->
  ClientState {- ^ client state -} ->
  IO ClientState
doNetworkClose networkId time st =
  do let (cs,st') = removeNetwork networkId st
         msg = ClientMessage
                 { _msgTime    = time
                 , _msgNetwork = view csNetwork cs
                 , _msgBody    = NormalBody "connection closed"
                 }
     return (recordNetworkMessage msg st')


-- | Respond to a network connection closing abnormally.
doNetworkError ::
  NetworkId     {- ^ failed network     -} ->
  ZonedTime     {- ^ current time       -} ->
  SomeException {- ^ termination reason -} ->
  ClientState   {- ^ client state       -} ->
  IO ClientState
doNetworkError networkId time ex st =
  do let (cs,st1) = removeNetwork networkId st
         st2 = foldl' (\acc msg -> recordError time cs (Text.pack msg) acc) st1
             $ exceptionToLines ex
     reconnectLogic ex cs st2

reconnectLogic ::
  SomeException {- ^ thread failure reason -} ->
  NetworkState  {- ^ failed network        -} ->
  ClientState   {- ^ client state          -} ->
  IO ClientState
reconnectLogic ex cs st

  | shouldReconnect =
      do (attempts, mbDisconnectTime) <- computeRetryInfo
         addConnection attempts mbDisconnectTime (view csNetwork cs) st

  | otherwise = return st

  where
    computeRetryInfo =
      case view csPingStatus cs of
        PingConnecting n tm                   -> pure (n+1, tm)
        _ | Just tm <- view csLastReceived cs -> pure (1, Just tm)
          | otherwise                         -> do now <- getCurrentTime
                                                    pure (1, Just now)

    reconnectAttempts = view (csSettings . ssReconnectAttempts) cs

    shouldReconnect =
      case view csPingStatus cs of
        PingConnecting n _ | n == 0 || n > reconnectAttempts          -> False
        _ | Just ConnectionFailure{}  <-             fromException ex -> True
          | Just HostnameResolutionFailure{} <-      fromException ex -> True
          | Just PingTimeout         <-              fromException ex -> True
          | Just ResourceVanished    <- ioe_type <$> fromException ex -> True
          | Just NoSuchThing         <- ioe_type <$> fromException ex -> True
          | otherwise                                                 -> False


-- | Respond to an IRC protocol line. This will parse the message, updated the
-- relevant connection state and update the UI buffers.
doNetworkLine ::
  NetworkId   {- ^ Network ID of message            -} ->
  ZonedTime   {- ^ current time                     -} ->
  ByteString  {- ^ Raw IRC message without newlines -} ->
  ClientState {- ^ client state                     -} ->
  IO ClientState
doNetworkLine networkId time line st =
  case view (clientConnections . at networkId) st of
    Nothing -> error "doNetworkLine: Network missing"
    Just cs ->
      let network = view csNetwork cs in
      case parseRawIrcMsg (asUtf8 line) of
        Nothing ->
          do let msg = Text.pack ("Malformed message: " ++ show line)
             return $! recordError time cs msg st

        Just raw ->
          do (st1,passed) <- clientPark st $ \ptr ->
                               notifyExtensions ptr network raw
                                 (view (clientExtensions . esActive) st)


             if not passed then return st1 else do

             let time' = computeEffectiveTime time (view msgTags raw)

                 (stateHook, viewHook)
                      = over both applyMessageHooks
                      $ partition (view messageHookStateful)
                      $ lookups
                          (view csMessageHooks cs)
                          messageHooks

             case stateHook (cookIrcMsg raw) of
               Nothing  -> return st1 -- Message ignored
               Just irc ->
                 do -- state with message recorded
                    -- record messages *before* applying state changes
                    let st2 =
                          case viewHook irc of
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

                    let (replies, st3) = applyMessageToClientState time irc networkId cs st2

                    traverse_ (sendMsg cs) replies
                    clientResponse time' irc cs st3


-- | Client-level responses to specific IRC messages.
-- This is in contrast to the connection state tracking logic in
-- "Client.NetworkState"
clientResponse :: ZonedTime -> IrcMsg -> NetworkState -> ClientState -> IO ClientState
clientResponse now irc cs st =
  case irc of
    Reply RPL_WELCOME _ ->
      -- run connection commands with the network focused and restore it afterward
      do let focus = NetworkFocus (view csNetwork cs)
         st' <- foldM (processConnectCmd now cs)
                      (set clientFocus focus st)
                      (view (csSettings . ssConnectCmds) cs)
         return $! set clientFocus (view clientFocus st) st'

    Authenticate challenge
      | AS_EcdsaWaitChallenge <- view csAuthenticationState cs ->
         processSaslEcdsa now challenge cs st

    _ -> return st


processSaslEcdsa ::
  ZonedTime    {- ^ message time  -} ->
  Text         {- ^ challenge     -} ->
  NetworkState {- ^ network state -} ->
  ClientState  {- ^ client state  -} ->
  IO ClientState
processSaslEcdsa now challenge cs st =
  case view ssSaslEcdsaFile ss of
    Nothing ->
      do sendMsg cs ircCapEnd
         return $! recordError now cs "panic: ecdsatool malformed output" st

    Just path ->
      do res <- Ecdsa.computeResponse path challenge
         case res of
           Left e ->
             do sendMsg cs ircCapEnd
                return $! recordError now cs (Text.pack e) st
           Right resp ->
             do sendMsg cs (ircAuthenticate resp)
                return $! set asLens AS_None st
  where
    ss = view csSettings cs
    asLens = clientConnections . ix (view csNetworkId cs) . csAuthenticationState


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
parseZncTime = parseTimeM True defaultTimeLocale
             $ iso8601DateFormat (Just "%T%Q%Z")


-- | Returns the list of values that were stored at the given indexes, if
-- a value was stored at that index.
lookups :: Ixed m => [Index m] -> m -> [IxValue m]
lookups ks m = mapMaybe (\k -> preview (ix k) m) ks


-- | Update the height and width fields of the client state
updateTerminalSize :: Vty -> ClientState -> IO ClientState
updateTerminalSize vty st =
  do (w,h) <- displayBounds (outputIface vty)
     return $! set clientWidth  w
            $  set clientHeight h st

-- | Respond to a VTY event.
doVtyEvent ::
  Vty                    {- ^ vty handle            -} ->
  Event                  {- ^ vty event             -} ->
  ClientState            {- ^ client state          -} ->
  IO (Maybe ClientState) {- ^ nothing when finished -}
doVtyEvent vty vtyEvent st =
  case vtyEvent of
    EvKey k modifier ->
      let cfg      = view clientConfig st
          keymap   = view configKeyMap       cfg
          winnames = view configWindowNames  cfg
          winmods  = view configJumpModifier cfg
          action = keyToAction keymap winmods winnames modifier k
      in doAction vty action st
    -- ignore event parameters due to raw TChan use
    EvResize{} -> Just <$> updateTerminalSize vty st
    EvPaste utf8 ->
       do let str = Text.unpack (Text.decodeUtf8With Text.lenientDecode utf8)
          return $! Just $! over clientTextBox (Edit.insertPaste str) st
    _ -> return (Just st)


-- | Map keyboard inputs to actions in the client
doAction ::
  Vty         {- ^ vty handle     -} ->
  Action      {- ^ action         -} ->
  ClientState {- ^ client state   -} ->
  IO (Maybe ClientState)
doAction vty action st =

  let continue !out -- detect when chains of M-a are broken
        | action == ActJumpToActivity =
            let upd Nothing = Just $! view clientFocus st
                upd x       = x
            in return $! Just $! over clientActivityReturn upd out
        | otherwise = return (Just out)

      changeEditor  f = continue (over clientTextBox f st)
      changeContent f = changeEditor
                      $ over Edit.content f
                      . set  Edit.lastOperation Edit.OtherOperation

      mbChangeEditor f =
        case traverseOf clientTextBox f st of
          Nothing -> continue $! set clientBell True st
          Just st' -> continue st'
  in
  case action of
    -- movements
    ActHome              -> changeEditor Edit.home
    ActEnd               -> changeEditor Edit.end
    ActLeft              -> changeContent Edit.left
    ActRight             -> changeContent Edit.right
    ActBackWord          -> changeContent Edit.leftWord
    ActForwardWord       -> changeContent Edit.rightWord

    -- edits
    ActKillHome          -> changeEditor Edit.killHome
    ActKillEnd           -> changeEditor Edit.killEnd
    ActKillWordBack      -> changeEditor (Edit.killWordBackward True)
    ActKillWordForward   -> changeEditor (Edit.killWordForward True)
    ActYank              -> changeEditor Edit.yank
    ActToggle            -> changeContent Edit.toggle
    ActDelete            -> changeContent Edit.delete
    ActBackspace         -> changeContent Edit.backspace

    -- special inserts
    ActBold              -> changeEditor (Edit.insert '\^B')
    ActColor             -> changeEditor (Edit.insert '\^C')
    ActItalic            -> changeEditor (Edit.insert '\^]')
    ActUnderline         -> changeEditor (Edit.insert '\^_')
    ActClearFormat       -> changeEditor (Edit.insert '\^O')
    ActReverseVideo      -> changeEditor (Edit.insert '\^V')
    ActDigraph           -> mbChangeEditor Edit.insertDigraph
    ActInsertEnter       -> changeEditor (Edit.insert '\^J')

    -- focus jumps
    ActJump i            -> continue (jumpFocus i st)
    ActJumpToActivity    -> continue (jumpToActivity st)
    ActJumpPrevious      -> continue (returnFocus st)
    ActRetreatFocus      -> continue (retreatFocus st)
    ActAdvanceFocus      -> continue (advanceFocus st)
    ActAdvanceNetwork    -> continue (advanceNetworkFocus st)

    ActReset             -> continue (changeSubfocus FocusMessages st)
    ActOlderLine         -> changeEditor $ \ed      -> fromMaybe ed $ Edit.earlier ed
    ActNewerLine         -> changeEditor $ \ed      -> fromMaybe ed $ Edit.later ed
    ActScrollUp          -> continue (scrollClient ( scrollAmount st) st)
    ActScrollDown        -> continue (scrollClient (-scrollAmount st) st)

    ActTabCompleteBack   -> doCommandResult False =<< tabCompletion True  st
    ActTabComplete       -> doCommandResult False =<< tabCompletion False st

    ActInsert c          -> changeEditor (Edit.insert c)
    ActEnter             -> doCommandResult True =<< executeInput st
    ActRefresh           -> refresh vty >> continue st
    ActCommand cmd       -> do resp <- executeUserCommand Nothing (Text.unpack cmd) st
                               case resp of
                                 CommandSuccess st1 -> continue st1
                                 CommandFailure st1 -> continue st1
                                 CommandQuit    _   -> return Nothing

    ActIgnored           -> continue st


-- | Process 'CommandResult' and update the 'ClientState' textbox
-- and error state. When quitting return 'Nothing'.
doCommandResult ::
  Bool          {- ^ clear on success -} ->
  CommandResult {- ^ command result   -} ->
  IO (Maybe ClientState)
doCommandResult clearOnSuccess res =
  let continue !st = return (Just st) in
  case res of
    CommandQuit    st -> Nothing <$ clientShutdown st
    CommandSuccess st -> continue (if clearOnSuccess then consumeInput st else st)
    CommandFailure st -> continue (set clientBell True st)


-- | Execute the the command on the first line of the text box
executeInput ::
  ClientState {- ^ client state -} ->
  IO CommandResult
executeInput st = execute (clientFirstLine st) st


-- | Respond to a timer event.
doTimerEvent ::
  NetworkId   {- ^ Network related to event -} ->
  TimedAction {- ^ Action to perform        -} ->
  ClientState {- ^ client state             -} ->
  IO ClientState
doTimerEvent networkId action =
  traverseOf
    (clientConnections . ix networkId)
    (applyTimedAction action)
