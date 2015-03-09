{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientState where

import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Lens
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.Functor
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Graphics.Vty.Image
import System.IO (Handle)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Irc.Format
import Irc.Model

import EditBox (EditBox)
import qualified EditBox as Edit

data ClientState = ClientState
  { _clientSendChan   :: TChan ByteString
  , _clientErrors     :: Maybe Handle
  , _clientConnection :: IrcConnection
  , _clientFocus      :: Focus
  , _clientDetailView :: !Bool
  , _clientEditBox    :: EditBox
  , _clientTabPattern :: Maybe String
  , _clientScrollPos :: Int
  , _clientHeight :: Int
  , _clientWidth :: Int
  , _clientIgnores :: !(Set Identifier)
  , _clientHighlights :: !(Set Text)
  , _clientMessages :: !(Map Identifier MessageList)
  , _clientNickColors :: [Color]
  , _clientAutomation :: [EventHandler]
  }

data MessageList = MessageList
  { _mlNewMessages :: !Int
  , _mlMentioned   :: !Bool
  , _mlMessages    :: [IrcMessage]
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
nextFocus = incrementFocus (+1)

-- | Advance the focus element backward. See 'incrementFocus' for
-- details.
prevFocus :: ClientState -> ClientState
prevFocus = incrementFocus (subtract 1)

-- | 'incrementFocus' allows moving forward and backward through
-- a sorted list of channel names and query windows. Information
-- windows like mask lists and info lists will always transition
-- back to the associated message view before moving forward
-- and backward. The server message window is placed at the
-- beginning of this rotation. In the case of overflow the focus
-- wraps around to the other side of the list.
incrementFocus :: (Int -> Int) -> ClientState -> ClientState
incrementFocus f st
  = clearTabPattern
  $ set clientScrollPos 0
  $ set clientFocus focus' st
  where
  focus' =
    case currentFocus of
      ChannelInfoFocus c -> ChannelFocus c
      MaskListFocus _  c -> ChannelFocus c
      ChannelFocus c     -> ChannelFocus (nextChannel c)

  focuses = Map.keys (fullMessageLists st)

  currentFocus = view clientFocus st

  -- TODO: fix this for case insensitivity
  nextChannel c =
    case elemIndex c focuses of
      Just i  -> focuses !! mod (f i) (length focuses)
      Nothing -> ""

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
  aux = case views mesgType isRelevant message of
          Nothing -> over mlMessages (cons message)
          Just txt -> over mlNewMessages (+1)
                    . over mlMentioned   (|| mention txt)
                    . over mlMessages (cons message)

  nickTxt = asUtf8 (idDenote (view (clientConnection . connNick) st))

  highlights
    = set (contains nickTxt) True
    $ view clientHighlights st

  mention txt =
    or [ Text.isInfixOf term (CI.foldCase txt)
       | term <- Set.toList highlights
       ]

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
