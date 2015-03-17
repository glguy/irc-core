{-# LANGUAGE OverloadedStrings #-}
{- | Administrative channel operations -}
module Moderation where

import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Text (Text)
import Data.List (nub, delete, elemIndex)
import Data.List.Split (chunksOf)
import Data.Monoid
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as Text

import Irc.Cmd
import Irc.Format
import Irc.Model
import Irc.Message

import ClientState

deopWaitDuration :: NominalDiffTime
deopWaitDuration = 5*60 -- seconds

-- | Perform a privileged operation. If the connection doesn't
-- already have +o on the channel it will be requested from
-- ChanServ and the privileged operation will be scheduled to
-- run when the connection gets +o.
doWithOps ::
  Identifier {- ^ channel -} ->
  (ClientState -> IO ClientState) {- ^ privileged operation -} ->
  ClientState -> IO ClientState
doWithOps = doWithOps' False

doWithOps' ::
  Bool {- ^ permanent change -} ->
  Identifier {- ^ channel -} ->
  (ClientState -> IO ClientState) {- ^ privileged operation -} ->
  ClientState -> IO ClientState
doWithOps' perm chan privop st
    | initiallyOp = finishUp st
    | otherwise = getOpFirst

  where
  conn = view clientConnection st
  myNick = view connNick conn

  -- was I op when the command was entered
  initiallyOp = nickHasModeInChannel myNick 'o' chan conn

  handler = EventHandler
    { _evName = "Get op for privop"
    , _evOnEvent = \evTgt evMsg evSt ->
         case view mesgType evMsg of
           ModeMsgType True 'o' modeNick
             | mkId modeNick == myNick
             , evTgt    == chan -> finishUp evSt
           _ -> return (over clientAutomation (cons handler) evSt)
    }

  finishUp st1 = privop =<< installTimer st1

  getOpFirst =
    do clientSend (privMsgCmd "chanserv" ("op " <> idDenote chan)) st
       return (over clientAutomation (cons handler) st)

  computeDeopTime =
    do now <- getCurrentTime
       return (addUTCTime deopWaitDuration now)

  installTimer st0

    | perm && deopScheduled chan st0 =
         return $ filterTimerEvents (/= DropOperator chan) st0

    | perm = return st0

    | deopScheduled chan st0 || not initiallyOp =
         do time <- computeDeopTime
            return $ addTimerEvent time (DropOperator chan)
                   $ filterTimerEvents (/= DropOperator chan) st0

    | otherwise = return st0

-- | Predicate to determine if a deop is scheduled to happen
deopScheduled ::
  Identifier {- ^ channel -} ->
  ClientState -> Bool
deopScheduled = elemOf (clientTimers . folded . folded . _DropOperator)

doAutoKickBan ::
  Identifier {- ^ channel -} ->
  Identifier {- ^ nick    -} ->
  Text       {- ^ reason  -} ->
  ClientState -> IO ClientState
doAutoKickBan chan nick reason st =
  -- TODO: Look up account name or hostname!
  do clientSend (modeCmd chan ["+b",banMask]) st
     clientSend (kickCmd chan nick (Text.encodeUtf8 reason)) st
     return st

  where
  usr  = view (clientConnection . connUsers . at nick) st
  nickMask = idDenote nick <> "!*@*"
  banMask = fromMaybe nickMask
          $ previews (folded . usrAccount . folded) ("$a:"<>) usr
        <|> previews (folded . usrHost    . folded) ("*!*@"<>) usr

-- | Cancel any pending deop timer if I'm deopped
cancelDeopTimerOnDeop :: EventHandler
cancelDeopTimerOnDeop = EventHandler
  { _evName = "cancel deop timer on deop"
  , _evOnEvent = \evTgt evMsg evSt ->
      let evSt' = reschedule evSt in
      case view mesgType evMsg of
        ModeMsgType False 'o' modeNick
          | mkId modeNick == view (clientConnection.connNick) evSt ->
                return $ filterTimerEvents (/= DropOperator evTgt) evSt'

        _ -> return evSt'
  }
  where
  reschedule = over clientAutomation (cons cancelDeopTimerOnDeop)

doOp :: ClientState -> [Identifier] -> IO ClientState
doOp st nicks
  | Just chan <- focusedChan st =

      doWithOps'
        (null nicks || myNick `elem` nicks) -- permanent?
        chan
        (massModeChange True 'o' chan (nub (delete myNick nicks)))
        (clearInput st)

  | otherwise = return st

  where
  myNick = view (clientConnection.connNick) st


doDeop :: ClientState -> [Identifier] -> IO ClientState
doDeop st nicks
  | Just chan <- focusedChan st =

      doWithOps
        chan
        (massModeChange False 'o' chan nicks')
        (clearInput st)

  | otherwise = return st

  where
  -- deop myself last
  nicks'
    | null nicks = [myNick]
    | myNick `elem` nicks = nub (delete myNick nicks) ++ [myNick]
    | otherwise = nicks

  myNick = view (clientConnection.connNick) st

doVoice :: ClientState -> [Identifier] -> IO ClientState
doVoice st nicks
  | Just chan <- focusedChan st =
      doWithOps
        chan
        (massModeChange True 'v' chan nicks')
        (clearInput st)
  | otherwise = return st
  where
  nicks'
    | null nicks = [view (clientConnection.connNick) st]
    | otherwise  = nub nicks

doDevoice :: ClientState -> [Identifier] -> IO ClientState
doDevoice st nicks
  | Just chan <- focusedChan st =
      doWithOps
        chan
        (massModeChange False 'v' chan nicks')
        (clearInput st)
  | otherwise = return st
  where
  nicks'
    | null nicks = [view (clientConnection.connNick) st]
    | otherwise  = nub nicks

massModeChange ::
  Bool       {- ^ polarity -} ->
  Char       {- ^ mode     -} ->
  Identifier {- ^ channel  -} ->
  [Identifier] {- ^ nicks -} ->
  ClientState -> IO ClientState
massModeChange polarity mode chan nicks st =
  do let nickChunks = chunksOf (view (clientConnection.connModes) st) nicks
     for_ nickChunks $ \nickChunk ->
       clientSend (modeCmd chan (modeArg (length nickChunk) : map idBytes nickChunk)) st
     return st
  where
  polarityBs
    | polarity  = B8.empty
    | otherwise = B8.singleton '-'

  modeArg n = polarityBs <> B8.replicate n mode

doTopicCmd ::
  ByteString {- ^ new topic -} ->
  ClientState -> IO ClientState
doTopicCmd topic st
  | not (B8.null topic)
  , Just chan <- focusedChan st =
     let go st' = st' <$ clientSend (topicCmd chan topic) st' in
     case preview (clientConnection . connChannels . ix chan . chanModes . folded) st of
       -- check if it's known that the mode isn't +t
       Just modes | hasn't (ix 't') modes -> go (clearInput st)
       _                                  -> doWithOps chan go (clearInput st)
  | otherwise = return st


doInvite ::
  ClientState ->
  Identifier {- ^ nickname -} ->
  IO ClientState
doInvite st nick =
  case focusedChan st of
    Nothing -> return st
    Just chan
      -- 'g' is the "FREEINVITE" mode, don't check for ops
      | channelHasMode chan 'g' (view clientConnection st) -> go (clearInput st)

      -- it's an error to invite someone already in channel
      | has (clientConnection . connChannels . ix chan . chanUsers . ix nick) st -> return st

      | otherwise -> doWithOps chan go (clearInput st)
      where
      go st' = st' <$ clientSend (inviteCmd nick chan) st'


doKick ::
  ClientState               ->
  Identifier {- ^ nick   -} ->
  Text       {- ^ reason -} ->
  IO ClientState
doKick st nick msg
  | Just chan <- focusedChan st =
      doWithOps chan (\evSt ->
        evSt <$ clientSend (kickCmd chan nick (Text.encodeUtf8 msg)) evSt) (clearInput st)
  | otherwise = return st


doRemove ::
  ClientState               ->
  Identifier {- ^ nick   -} ->
  Text       {- ^ reason -} ->
  IO ClientState
doRemove st nick msg
  | Just chan <- focusedChan st =
      doWithOps chan (\evSt ->
        evSt <$ clientSend (removeCmd chan nick (Text.encodeUtf8 msg)) evSt) (clearInput st)
  | otherwise = return st
