{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module ClientState where

import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.DeepSeq (force)
import Control.Lens
import Control.Monad (foldM, guard)
import Data.ByteString (ByteString)
import Data.Char (isControl)
import Data.Functor
import Data.Functor.Compose
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (TimeZone, UTCTime)
import Graphics.Vty.Image
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Irc.Format
import Irc.Message
import Irc.Model

import EditBox (EditBox)
import qualified EditBox as Edit
import ImageUtils (cleanText, nameHighlighter)

data ClientState = ClientState
  { _clientSendChan   :: TChan ByteString
  , _clientErrors     :: Maybe Handle
  , _clientConnection :: IrcConnection
  , _clientFocus      :: Focus
  , _clientDetailView :: !Bool
  , _clientTimeView   :: !Bool
  , _clientMetaView   :: !Bool
  , _clientEditBox    :: EditBox
  , _clientUserInfo   :: ByteString -- ctcp standard says users has to set this
  , _clientTabPattern :: Maybe String
  , _clientScrollPos :: Int
  , _clientHeight :: Int
  , _clientWidth :: Int
  , _clientIgnores :: !(Set Identifier) -- Todo: support mask matching
  , _clientHighlights :: !(Set ByteString)
  , _clientMessages :: !(Map Identifier MessageList)
  , _clientNickColors :: [Color]
  , _clientAutomation :: [EventHandler]
  , _clientTimers     :: Map UTCTime [TimerEvent]
  , _clientTimeZone   :: TimeZone
  }
  -- TODO: split this record into logical pieces

data TimerEvent
  = DropOperator Identifier
  deriving (Read, Show, Eq)

data MessageList = MessageList
  { _mlNewMessages :: !Int
  , _mlMentioned   :: !Bool
  , _mlMessages    :: [(IrcMessage,Image)]
  }

defaultMessageList :: MessageList
defaultMessageList = MessageList
  { _mlNewMessages = 0
  , _mlMentioned   = False
  , _mlMessages    = []
  }

data Focus
  = ChannelFocus Identifier
  | ChannelInfoFocus Identifier
  | MaskListFocus Char Identifier
  deriving (Eq, Ord, Read, Show)

data EventHandler = EventHandler
  { _evName :: String
  , _evOnEvent :: Identifier -> IrcMessage -> ClientState -> IO ClientState
  }

makeLenses ''ClientState
makeLenses ''MessageList
makeLenses ''EventHandler
makePrisms ''Focus
makePrisms ''TimerEvent

resetCurrentChannelMessages :: ClientState -> ClientState
resetCurrentChannelMessages st =
  over (clientMessages . ix (focusedName st))
       ( set mlNewMessages 0
       . set mlMentioned False
       )
       st

-- Return the message part of a message which counts
-- toward unread message count.
isRelevant :: IrcMessageType -> Maybe Text
isRelevant (PrivMsgType   msg) = Just msg
isRelevant (NoticeMsgType msg) = Just msg
isRelevant (ActionMsgType msg) = Just msg
isRelevant (ErrorMsgType msg)  = Just msg
isRelevant _                   = Nothing


clientInput :: ClientState -> String
clientInput = view (clientEditBox . Edit.content)

clearInput :: ClientState -> ClientState
clearInput
  = clearTabPattern
  . over clientEditBox Edit.success

-- | Advance the focus element forward. See 'incrementFocus' for
-- details.
nextFocus :: ClientState -> ClientState
nextFocus = incrementFocus nextInSorted

-- | Advance the focus element backward. See 'incrementFocus' for
-- details.
prevFocus :: ClientState -> ClientState
prevFocus = incrementFocus prevInSorted

-- | Jump to a zero-based index in the set of focus targets.
jumpFocus :: Int -> ClientState -> ClientState
jumpFocus i = incrementFocus $ \current targets ->
  if 0 <= i && i < Set.size targets
    then Set.elemAt i targets
    else current

-- | Find a channel to jump to that is either marked for mention
-- or which has new messages
jumpActivity :: ClientState -> ClientState
jumpActivity st =
  case active of
    []     -> st
    name:_ -> set clientFocus (ChannelFocus name) st
  where
  active =
    [ name | (name,messages) <- views clientMessages Map.toList st
           , view mlMentioned messages ] ++
    [ name | (name,messages) <- views clientMessages Map.toList st
           , view mlNewMessages messages > 0 ]

-- | 'incrementFocus' allows moving forward and backward through
-- a sorted list of channel names and query windows. Information
-- windows like mask lists and info lists will always transition
-- back to the associated message view before moving forward
-- and backward. The server message window is placed at the
-- beginning of this rotation. In the case of overflow the focus
-- wraps around to the other side of the list.
incrementFocus ::
  (Identifier -> Set Identifier -> Identifier) ->
  ClientState -> ClientState
incrementFocus f st
  = clearTabPattern
  $ set clientScrollPos 0
  $ set clientFocus focus' st
  where
  focus' =
    case view clientFocus st of
      ChannelInfoFocus c -> ChannelFocus c
      MaskListFocus _  c -> ChannelFocus c
      ChannelFocus c     -> ChannelFocus (f c focuses)

  focuses = Map.keysSet (fullMessageLists st)

clearTabPattern :: ClientState -> ClientState
clearTabPattern = set clientTabPattern Nothing

clientSend :: ByteString -> ClientState -> IO ()
clientSend x st = atomically (writeTChan (view clientSendChan st) x)

focusedName :: ClientState -> Identifier
focusedName st =
  case view clientFocus st of
    ChannelInfoFocus c -> c
    MaskListFocus _  c -> c
    ChannelFocus     c -> c

focusedChan :: ClientState -> Maybe Identifier
focusedChan st =
  case view clientFocus st of
    ChannelInfoFocus c -> Just c
    MaskListFocus _  c -> Just c
    ChannelFocus     c
      | isChannelName c (view clientConnection st) -> Just c
      | otherwise -> Nothing

addMessage :: Identifier -> IrcMessage -> ClientState -> ClientState
addMessage target message st
  | view (clientConnection . connNick) st == target =
      over (clientMessages . at (views mesgSender userNick message))
           (Just . aux . fromMaybe defaultMessageList)
           st
  | otherwise =
      over (clientMessages . at target)
           (Just . aux . fromMaybe defaultMessageList)
           st
  where
  conn = view clientConnection st

  aux = case views mesgType isRelevant message of
          Nothing -> over mlMessages (cons (message,error "unused colored message"))
          Just txt -> over mlNewMessages (+1)
                    . over mlMentioned   (|| mention txt || private)
                    . over mlMessages (cons (message,coloredImage))
            where
            !coloredImage
              | Text.any isControl txt = cleanText txt
              | otherwise = force -- avoid holding on to old channel lists
                          $ nameHighlighter
                                (Text.encodeUtf8 txt)
                                (views (connChannels . ix target . chanUsers) Map.keysSet conn)
                                (view connNick conn)
                                (view clientNickColors st)

  nickTxt = idDenote (view connNick conn)

  highlights
    = set (contains nickTxt) True
    $ view clientHighlights st

  mention txt =
    or [ B.isInfixOf term (ircFoldCase (Text.encodeUtf8 txt))
       | term <- Set.toList highlights
       ]

  private = isNickName target conn && not (view mesgMe message)

fullMessageLists :: ClientState -> Map Identifier MessageList
fullMessageLists st
   = view clientMessages st
  <> views (clientConnection . connChannels)
           (defaultMessageList <$)
           st
  <> Map.singleton "" defaultMessageList

runEventHandlers :: Identifier -> IrcMessage -> ClientState -> IO ClientState
runEventHandlers tgt msg st0 = foldM aux st1 hs
  where
  st1 = set clientAutomation [] st0
  hs  = view clientAutomation st0

  aux st h = view evOnEvent h tgt msg st

nextInSorted :: Ord a => a -> Set a -> a
nextInSorted x ys =
  case Set.lookupGT x ys of
    Just y  -> y
    Nothing ->
     case Set.minView ys of
       Just (y,_) -> y
       Nothing    -> x

prevInSorted :: Ord a => a -> Set a -> a
prevInSorted x ys =
  case Set.lookupLT x ys of
    Just y  -> y
    Nothing ->
     case Set.maxView ys of
       Just (y,_) -> y
       Nothing    -> x

nextTimerEvent :: UTCTime -> ClientState -> Maybe (TimerEvent, ClientState)
nextTimerEvent now = alaf Compose clientTimers aux
  where
  aux :: Map UTCTime [TimerEvent] -> Maybe (TimerEvent, Map UTCTime [TimerEvent])
  aux timers =
    do ((when,events), timers1) <- Map.minViewWithKey timers
       guard (when <= now)
       case events of
         []   -> error "nextTimerEvent: empty entry!"
         [e]  -> return (e, timers1)
         e:es -> return (e, Map.insert when es timers1)

filterTimerEvents :: (TimerEvent -> Bool) -> ClientState -> ClientState
filterTimerEvents p = over clientTimers (Map.mapMaybe aux)
  where
  aux xs
    | null xs' = Nothing
    | otherwise = Just xs'
    where xs' = filter p xs

addTimerEvent :: UTCTime -> TimerEvent -> ClientState -> ClientState
addTimerEvent when e = over clientTimers (Map.insertWith (++) when [e])
