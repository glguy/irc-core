{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientState where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Lens
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import System.IO (Handle)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as Text

import Irc.Model
import Irc.List (List)
import qualified Irc.List as List

import EditBox (EditBox)
import qualified EditBox as Edit

data ClientState = ClientState
  { _clientSendChan   :: TChan ByteString
  , _clientErrors     :: Handle
  , _clientConnection :: IrcConnection
  , _clientFocus      :: Focus
  , _clientDetailView :: !Bool
  , _clientExtraWhitespace :: !Bool
  , _clientEditBox    :: EditBox
  , _clientTabPattern :: Maybe String
  , _clientScrollPos :: Int
  , _clientHeight :: Int
  , _clientWidth :: Int
  , _clientMessagesSeen :: !(Map (CI ByteString) SeenMetrics)
  , _clientIgnores :: !(Set (CI ByteString))
  }

data Focus
  = ChannelFocus ByteString
  | ChannelInfoFocus ByteString
  | MaskListFocus Char ByteString
  | ServerFocus
  deriving (Eq, Ord, Read, Show)

data SeenMetrics = SeenMetrics
  { _seenNewMessages :: !Int
  , _seenTotalMessages :: !Int
  , _seenMentioned :: !Bool
  }
  deriving (Read,Show)

defaultSeenMetrics :: SeenMetrics
defaultSeenMetrics = SeenMetrics
  { _seenNewMessages = 0
  , _seenTotalMessages = 0
  , _seenMentioned = False
  }

makeLenses ''ClientState
makePrisms ''Focus
makeLenses ''SeenMetrics

focusMessages :: Applicative f => Focus -> LensLike' f IrcConnection (List IrcMessage)
focusMessages x f conn = case x of
  ChannelFocus c
    | isChannelName c conn -> (connChannelIx c . chanMessages) f conn
    | otherwise            -> (connUserIx    c . usrMessages ) f conn
  ServerFocus              -> connMessages                     f conn
  MaskListFocus _ _        -> ignored                          f conn
  ChannelInfoFocus _       -> ignored                          f conn

isChannelName :: ByteString -> IrcConnection -> Bool
isChannelName c conn =
  case B.uncons c of
    Just (x,_) -> x `elem` view connChanTypes conn
    _ -> False -- probably shouldn't happen

resetCurrentChannelMessages :: ClientState -> ClientState
resetCurrentChannelMessages st =
  case view clientFocus st of
    ChannelFocus c -> clear c  st
    ServerFocus    -> clear "" st
    _              -> st

  where
  clear c = over ( clientMessagesSeen . ix (CI.mk c))
                 ( set seenNewMessages 0
                 . set seenMentioned False
                 )

updateNewMessages :: ClientState -> ClientState
updateNewMessages st
  = resetCurrentChannelMessages
  $ over clientMessagesSeen aux st
  where
  me = CI.foldCase (asUtf8 (view (clientConnection.connNick) st))
  combine = updateSeen me

  aux m0 =
    Map.mergeWithKey
      (\_ x y -> Just (combine x y))
      (const mempty) -- drop the counts that aren't relevant
      (fmap (combine defaultSeenMetrics)) -- default value of none seen
      m0
      (serverCounts <> channelCounts <> userCounts)

  serverCounts
    = Map.singleton "" (view (clientConnection . connMessages) st)

  channelCounts
    = fmap (view chanMessages)
           (view (clientConnection . connChannels) st)

  userCounts
    = Map.filter ((>0) . List.length)
    $ fmap (view usrMessages)
           (view (clientConnection . connUsers) st)

updateSeen :: Text -> SeenMetrics -> List IrcMessage -> SeenMetrics
updateSeen me seen msgs
    = over seenNewMessages (+ length additionalMessages)
    $ over seenMentioned   (|| mention)
    $ set  seenTotalMessages totalMessages seen
    where
    totalMessages = List.length msgs
    additionalMessages
      = mapMaybe (views mesgType isRelevant)
      $ take (totalMessages - view seenTotalMessages seen)
      $ toList msgs

    mention = any (Text.isInfixOf me) (map CI.foldCase additionalMessages)


-- Return the message part of a message which counts
-- toward unread message count.
isRelevant :: IrcMessageType -> Maybe Text
isRelevant (PrivMsgType   msg) = Just msg
isRelevant (NoticeMsgType msg) = Just msg
isRelevant (ActionMsgType msg) = Just msg
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
      _                  -> nextChannel

  focuses = ServerFocus
          : map ChannelFocus
          ( views clientConnection activeChannelNames st
         ++ views clientConnection activeUserNames st )

  currentFocus = view clientFocus st

  -- TODO: fix this for case insensitivity
  nextChannel =
    case elemIndex currentFocus focuses of
      Just i  -> focuses !! mod (f i) (length focuses)
      Nothing -> ServerFocus

clearTabPattern :: ClientState -> ClientState
clearTabPattern = set clientTabPattern Nothing

clientSend :: ByteString -> ClientState -> IO ()
clientSend x st = atomically (writeTChan (view clientSendChan st) x)

focusedName :: ClientState -> Maybe ByteString
focusedName st =
  case view clientFocus st of
    ServerFocus -> Nothing
    ChannelInfoFocus c -> Just c
    MaskListFocus _  c -> Just c
    ChannelFocus     c -> Just c
