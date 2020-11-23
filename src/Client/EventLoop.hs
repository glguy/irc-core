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
  , ClientEvent(..)
  ) where

import           Client.CApi (ThreadEntry, popTimer)
import           Client.Commands
import           Client.Configuration (configJumpModifier, configKeyMap, configWindowNames, configDownloadDir)
import           Client.Configuration.ServerSettings
import           Client.EventLoop.Actions
import           Client.EventLoop.Errors (exceptionToLines)
import           Client.EventLoop.Network (clientResponse)
import           Client.Hook
import           Client.Image
import           Client.Image.Layout (scrollAmount)
import           Client.Image.StatusLine (clientTitle)
import           Client.Log
import           Client.Message
import           Client.Network.Async
import           Client.State
import           Client.State.DCC
import qualified Client.State.EditBox     as Edit
import           Client.State.Extensions
import           Client.State.Focus
import           Client.State.Network
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Foldable
import           Data.Traversable
import           Data.List
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Maybe
import           Data.Ord
import           Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import           Data.Time
import           GHC.IO.Exception (IOErrorType(..), ioe_type)
import           Graphics.Vty
import           Irc.Message
import           Irc.Codes
import           Irc.RawIrcMsg
import           LensUtils
import           Hookup (ConnectionFailure(..))


-- | Sum of the five possible event types the event loop handles
data ClientEvent
  = VtyEvent Event                        -- ^ Key presses and resizing
  | NetworkEvents (NonEmpty (Text, NetworkEvent)) -- ^ Incoming network events
  | TimerEvent Text TimedAction      -- ^ Timed action and the applicable network
  | ExtTimerEvent Int                     -- ^ extension ID
  | DCCUpdate DCCUpdate                   -- ^ Event on any transfer
  | ThreadEvent Int ThreadEntry


-- | Block waiting for the next 'ClientEvent'. This function will compute
-- an appropriate timeout based on the current connections.
getEvent ::
  Vty         {- ^ vty handle   -} ->
  ClientState {- ^ client state -} ->
  IO ClientEvent
getEvent vty st =
  do timer <- prepareTimer
     atomically (asum [timer, vtyEvent, networkEvents, dccUpdate, threadJoin])
  where
    vtyEvent = VtyEvent <$> readTChan (_eventChannel (inputIface vty))

    networkEvents =
      do xs <- for (HashMap.toList (view clientConnections st)) $ \(network, conn) ->
           do ys <- recv (view csSocket conn)
              return (map ((,) network) ys)
         case nonEmpty (concat xs) of
           Just events1 -> return (NetworkEvents events1)
           Nothing      -> retry

    prepareTimer =
      case earliestEvent st of
        Nothing -> return retry
        Just (runAt,event) ->
          do now <- getCurrentTime
             let microsecs = truncate (1000000 * diffUTCTime runAt now)
             var <- registerDelay (max 0 microsecs)
             return $ do ready <- readTVar var
                         unless ready retry
                         return event

    dccUpdate = DCCUpdate <$> readTChan (view clientDCCUpdates st)

    threadJoin =
      do (i,r) <- readTQueue (view clientThreadJoins st)
         pure (ThreadEvent i r)

-- | Compute the earliest scheduled timed action for the client
earliestEvent :: ClientState -> Maybe (UTCTime, ClientEvent)
earliestEvent st = earliest2 networkEvent extensionEvent
  where
    earliest2 (Just (time1, action1)) (Just (time2, action2))
      | time1 < time2 = Just (time1, action1)
      | otherwise     = Just (time2, action2)
    earliest2 x y = mplus x y

    mkEventN (network, (time, action)) = (time, TimerEvent network action)
    networkEvent =
      minimumByOf
        (clientConnections . (ifolded <. folding nextTimedAction) . withIndex . to mkEventN)
        (comparing fst)
        st

    mkEventE (i, (time,_,_,_,_)) = (time, ExtTimerEvent i)
    extensionEvent =
      minimumByOf
        (clientExtensions . esActive . (ifolded <. folding popTimer) . withIndex . to mkEventE)
        (comparing fst)
        st

-- | Apply this function to an initial 'ClientState' to launch the client.
eventLoop :: Vty -> ClientState -> IO ()
eventLoop vty st =
  do when (view clientBell st) (beep vty)
     processLogEntries st

     let (pic, st') = clientPicture (clientTick st)
     update vty pic
     setWindowTitle vty (clientTitle st)

     event <- getEvent vty st'
     case event of
       ExtTimerEvent i ->
         eventLoop vty =<< clientExtTimer i st'
       ThreadEvent i result ->
         eventLoop vty =<< clientThreadJoin i result st'
       TimerEvent networkId action ->
         eventLoop vty =<< doTimerEvent networkId action st'
       VtyEvent vtyEvent ->
         traverse_ (eventLoop vty) =<< doVtyEvent vty vtyEvent st'
       NetworkEvents networkEvents ->
         eventLoop vty =<< foldM doNetworkEvent st' networkEvents
       DCCUpdate upd ->
         eventLoop vty =<< doDCCUpdate upd st'

-- | Apply a single network event to the client state.
doNetworkEvent :: ClientState -> (Text, NetworkEvent) -> IO ClientState
doNetworkEvent st (net, networkEvent) =
  case networkEvent of
    NetworkLine  time line -> doNetworkLine  net time line st
    NetworkError time ex   -> doNetworkError net time ex st
    NetworkOpen  time      -> doNetworkOpen  net time st
    NetworkTLS   txts      -> doNetworkTLS   net txts st
    NetworkClose time      -> doNetworkClose net time st

-- | Sound the terminal bell assuming that the @BEL@ control code
-- is supported.
beep :: Vty -> IO ()
beep = ringTerminalBell . outputIface

processLogEntries :: ClientState -> IO ()
processLogEntries =
  traverse_ writeLogLine . reverse . view clientLogQueue

-- | Respond to a network connection successfully connecting.
doNetworkOpen ::
  Text        {- ^ network name -} ->
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
         let cs' = cs & csLastReceived .~ (Just $! zonedTimeToUTC time)
         return $! recordNetworkMessage msg
                 $ setStrict (clientConnections . ix networkId) cs' st

-- | Update the TLS certificates for a connection
doNetworkTLS ::
  Text   {- ^ network name      -} ->
  [Text] {- ^ certificate lines -} ->
  ClientState ->
  IO ClientState
doNetworkTLS network cert st =
  pure $! over (clientConnections . ix network) upd st
  where
    upd = set csCertificate cert
        . set (csPingStatus . _PingConnecting . _3) NoRestriction

-- | Respond to a network connection closing normally.
doNetworkClose ::
  Text        {- ^ network name -} ->
  ZonedTime   {- ^ event time   -} ->
  ClientState {- ^ client state -} ->
  IO ClientState
doNetworkClose networkId time st =
  do let (cs,st1) = removeNetwork networkId st
         msg = ClientMessage
                 { _msgTime    = time
                 , _msgNetwork = view csNetwork cs
                 , _msgBody    = NormalBody "connection closed"
                 }
     return (recordNetworkMessage msg st1)


-- | Respond to a network connection closing abnormally.
doNetworkError ::
  Text          {- ^ failed network     -} ->
  ZonedTime     {- ^ current time       -} ->
  SomeException {- ^ termination reason -} ->
  ClientState   {- ^ client state       -} ->
  IO ClientState
doNetworkError networkId time ex st =
  do let (cs,st1) = removeNetwork networkId st
         st2 = foldl' (\acc msg -> recordError time (view csNetwork cs) (Text.pack msg) acc) st1
             $ exceptionToLines ex
     reconnectLogicOnFailure ex cs st2

reconnectLogicOnFailure ::
  SomeException {- ^ thread failure reason -} ->
  NetworkState  {- ^ failed network        -} ->
  ClientState   {- ^ client state          -} ->
  IO ClientState
reconnectLogicOnFailure ex cs st

  | shouldReconnect =
      do (attempts, mbDisconnectTime) <- computeRetryInfo
         addConnection attempts mbDisconnectTime Nothing (view csNetwork cs) st

  | otherwise = return st

  where
    computeRetryInfo =
      case view csPingStatus cs of
        PingConnecting n tm _                 -> pure (n+1, tm)
        _ | Just tm <- view csLastReceived cs -> pure (1, Just tm)
          | otherwise                         -> do now <- getCurrentTime
                                                    pure (1, Just now)

    reconnectAttempts = view (csSettings . ssReconnectAttempts) cs

    shouldReconnect =
      case view csPingStatus cs of
        PingConnecting n _ _ | n == 0 || n > reconnectAttempts        -> False
        _ | Just ConnectionFailure{}         <-      fromException ex -> True
          | Just HostnameResolutionFailure{} <-      fromException ex -> True
          | Just PingTimeout         <-              fromException ex -> True
          | Just ResourceVanished    <- ioe_type <$> fromException ex -> True
          | Just NoSuchThing         <- ioe_type <$> fromException ex -> True
          | otherwise                                                 -> False


-- | Respond to an IRC protocol line. This will parse the message, updated the
-- relevant connection state and update the UI buffers.
doNetworkLine ::
  Text        {- ^ Network name                     -} ->
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
        _ | PingConnecting _ _ WaitTLSRestriction <- view csPingStatus cs ->
          st <$ abortConnection StartTLSFailed (view csSocket cs)

        Just raw
          | PingConnecting _ _ StartTLSRestriction <- view csPingStatus cs ->
          startTLSLine networkId cs st raw

        Nothing ->
          do let msg = Text.pack ("Malformed message: " ++ show line)
             return $! recordError time network msg st

        Just raw ->
          do (st1,passed) <- clientNotifyExtensions network raw st

             if not passed then return st1 else do

             let time' = computeEffectiveTime time (view msgTags raw)

                 (stateHook, viewHook)
                      = over both applyMessageHooks
                      $ partition (view messageHookStateful)
                      $ view csMessageHooks cs

             case stateHook (cookIrcMsg raw) of
               Nothing  -> return st1 -- Message ignored
               Just irc ->
                 do -- state with message recorded
                    -- record messages *before* applying state changes
                    -- so that quits, nick-changes, etc (TargetUser) will dispatch to
                    -- the correct window
                    let st2 =
                          case viewHook irc of
                            Nothing -> st1 -- Message hidden
                            Just irc'
                              | hideMessage irc' -> st1
                              | otherwise -> recordIrcMessage network target msg st1
                              where
                                myNick = view csNick cs
                                target = msgTarget myNick irc
                                msg = ClientMessage
                                      { _msgTime    = time'
                                      , _msgNetwork = network
                                      , _msgBody    = IrcBody irc'
                                      }

                    let (replies, dccUp, st3) =
                          applyMessageToClientState time irc networkId cs st2

                    traverse_ (atomically . writeTChan (view clientDCCUpdates st3)) dccUp
                    traverse_ (sendMsg cs) replies
                    clientResponse time' irc cs st3

-- Highly restricted message handler for messages sent before STARTTLS
-- has completed.
startTLSLine :: Text -> NetworkState -> ClientState -> RawIrcMsg -> IO ClientState
startTLSLine network cs st raw =
  do now <- getZonedTime
     let irc = cookIrcMsg raw
         myNick = view csNick cs
         target = msgTarget myNick irc
         msg = ClientMessage
             { _msgTime    = now
             , _msgNetwork = network
             , _msgBody    = IrcBody irc
             }
         st1 = recordIrcMessage network target msg st

     case irc of
       Notice{} -> pure st1
       Reply RPL_STARTTLS _ ->
         do upgrade (view csSocket cs)
            pure (set ( clientConnections . ix network . csPingStatus
                      . _PingConnecting . _3)
                      WaitTLSRestriction st1)
       _ -> st1 <$ abortConnection StartTLSFailed (view csSocket cs)

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
        | otherwise = return $! Just
                     $! set clientActivityReturn Nothing out

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
    ActStrikethrough     -> changeEditor (Edit.insert '\^^')
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
    ActScrollUpSmall     -> continue (scrollClient ( 3) st)
    ActScrollDownSmall   -> continue (scrollClient (-3) st)

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


-- | Actions to be run when exiting the client.
clientShutdown :: ClientState -> IO ()
clientShutdown st = () <$ clientStopExtensions st
 -- other shutdown stuff might be added here later


-- | Execute the the command on the first line of the text box
executeInput ::
  ClientState {- ^ client state -} ->
  IO CommandResult
executeInput st = execute (clientFirstLine st) st


-- | Respond to a timer event.
doTimerEvent ::
  Text        {- ^ Network related to event -} ->
  TimedAction {- ^ Action to perform        -} ->
  ClientState {- ^ client state             -} ->
  IO ClientState
doTimerEvent networkId action =
  traverseOf
    (clientConnection networkId)
    (applyTimedAction action)

doDCCUpdate :: DCCUpdate -> ClientState -> IO ClientState
doDCCUpdate upd st0 =
  case upd of
    PercentUpdate k newVal -> return $ set (commonLens k . dtProgress) newVal st0
    SocketInterrupted k    -> reportKill k LostConnection
    UserInterrupted k      -> reportKill k UserKilled
    Finished k             -> reportKill k CorrectlyFinished
    accept@(Accept k _ _) ->
      do let dccState0 = view clientDCC st0
             dccState1 = acceptUpdate accept dccState0
             -- st1 = set clientDCC dccState1 st0

             updChan = view clientDCCUpdates st0
             mdir = view (clientConfig . configDownloadDir) st0

         dccState2 <- supervisedDownload mdir k updChan dccState1
         return $ set clientDCC dccState2 st0
  where
    reportKill k status = return $ over clientDCC (reportStopWithStatus k status) st0
    commonLens k = clientDCC . dsTransfers . at k . _Just
