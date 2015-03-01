{-# LANGUAGE TemplateHaskell #-}

module ClientState where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Data.ByteString (ByteString)
import Data.Monoid
import System.IO (Handle)
import Control.Lens
import Data.List (elemIndex)
import Data.Map (Map)
import Data.CaseInsensitive (CI)
import Data.Foldable (toList)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map

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
  , _clientDetailView :: Bool
  , _clientEditBox    :: EditBox
  , _clientTabPattern :: Maybe String
  , _clientInputHistory :: [String]
  , _clientInputHistoryPos :: Int
  , _clientScrollPos :: Int
  , _clientHeight :: Int
  , _clientWidth :: Int
  , _clientMessagesSeen :: Map (CI ByteString) Int
  }

data Focus
  = ChannelFocus ByteString
  | ChannelInfoFocus ByteString
  | UserFocus ByteString
  | BanListFocus ByteString
  | ServerFocus

makeLenses ''ClientState
makePrisms ''Focus

focusMessages :: Applicative f => Focus -> LensLike' f IrcConnection (List IrcMessage)
focusMessages x = case x of
  ChannelFocus c     -> connChannelIx c . chanMessages
  UserFocus    u     -> connUserIx u . usrMessages
  BanListFocus _     -> ignored
  ChannelInfoFocus _ -> ignored
  ServerFocus        -> ignored

updateNewMessages :: ClientState -> ClientState
updateNewMessages st =
  case view clientFocus st of
    ChannelFocus c ->
       set (clientMessagesSeen . at (CI.mk c))
           (preview (clientConnection . connChannelIx c . chanMessages . to List.length)
                    st)
           st
    UserFocus    u     ->
       set (clientMessagesSeen . at (CI.mk u))
           (preview (clientConnection . connUserIx u . usrMessages . to List.length)
                    st)
           st
    BanListFocus _     -> st
    ServerFocus        -> st
    ChannelInfoFocus _ -> st

countNewMessages :: ClientState -> Map (CI ByteString) Int
countNewMessages st =
  Map.mergeWithKey
    (\_ x y -> Just (combine x y))
    (const mempty) -- drop the counts that aren't relevant
    (fmap (combine 0)) -- default value of none seen
    (view clientMessagesSeen st)
    (channelCounts <> userCounts)
  where
  channelCounts
    = fmap (view chanMessages)
           (view (clientConnection . connChannels) st)

  userCounts
    = Map.filter ((>0) . List.length)
    $ fmap (view usrMessages)
           (view (clientConnection . connUsers) st)

  combine :: Int -> List IrcMessage -> Int
  combine seen msgs
    = length
    $ filter (views mesgType isRelevant)
    $ take (List.length msgs - seen)
    $ toList msgs

  isRelevant PrivMsgType   = True
  isRelevant NoticeMsgType = True
  isRelevant _             = False


clientInput :: ClientState -> String
clientInput = view (clientEditBox . Edit.content)

clearInput :: ClientState -> ClientState
clearInput = clearTabPattern
           . set clientEditBox Edit.empty

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
      UserFocus        u -> UserFocus u -- TODO
      ChannelInfoFocus c -> ChannelInfoFocus (nextChannel c)
      BanListFocus     c -> BanListFocus     (nextChannel c)
      ChannelFocus     c -> ChannelFocus     (nextChannel c)
  channels = views clientConnection activeChannelNames st
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
