{-# LANGUAGE TemplateHaskell #-}

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
  deriving (Read, Show)

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
    ChannelFocus c -> clear c st
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
      (channelCounts <> userCounts)

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

nextFocus :: ClientState -> ClientState
nextFocus = incrementFocus (+1)

prevFocus :: ClientState -> ClientState
prevFocus = incrementFocus (subtract 1)

incrementFocus :: (Int -> Int) -> ClientState -> ClientState
incrementFocus f st
  = set clientScrollPos 0
  $ set clientFocus focus' st
  where
  focus' =
    case view clientFocus st of
      ServerFocus ->
        case channels of
          []  -> ServerFocus
          c:_ -> ChannelFocus c
      ChannelInfoFocus c -> ChannelFocus c
      MaskListFocus _  c -> ChannelFocus c
      ChannelFocus     c -> ChannelFocus (nextChannel c)
  channels = views clientConnection activeChannelNames st
          ++ views clientConnection activeUserNames st
  nextChannel c
    | null channels = c
    | otherwise =
      case elemIndex c channels of
        Nothing -> head channels
        Just i  -> channels !! mod (f i) (length channels)

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
