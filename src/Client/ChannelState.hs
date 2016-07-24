{-# Language TemplateHaskell #-}

{-|
Module      : Client.ChannelState
Description : IRC channel session state
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module is responsible for tracking the state of an individual IRC
channel while the client is connected to it. When the client joins a
channel a new channel session is created and when the client leaves
a channel is it destroyed.
-}

module Client.ChannelState where

import           Control.Lens
import           Data.HashMap.Strict
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import           Irc.Identifier
import           Irc.UserInfo

data ChannelState = ChannelState
  { _chanTopic :: !Text
  , _chanTopicProvenance :: !(Maybe TopicProvenance)
  , _chanUsers :: !(HashMap Identifier String)
  , _chanModes :: !(Map Char Text)
  , _chanLists :: !(Map Char (HashMap Text (Text, UTCTime)))
  , _chanCreation :: !(Maybe UTCTime)
  }
  deriving Show

data TopicProvenance = TopicProvenance
  { _topicAuthor :: !UserInfo
  , _topicTime   :: !UTCTime
  }
  deriving Show

makeLenses ''ChannelState
makeLenses ''TopicProvenance

newChannel :: ChannelState
newChannel = ChannelState
  { _chanTopic = Text.empty
  , _chanTopicProvenance = Nothing
  , _chanUsers = HashMap.empty
  , _chanModes = Map.empty
  , _chanLists = Map.empty
  , _chanCreation = Nothing
  }

chanList :: Functor f => Char -> LensLike' f ChannelState (HashMap Text (Text, UTCTime))
chanList mode = chanLists . at mode . non' _Empty

joinChannel :: Identifier -> ChannelState -> ChannelState
joinChannel nick = set (chanUsers . at nick) (Just "")

partChannel :: Identifier -> ChannelState -> ChannelState
partChannel nick = set (chanUsers . at nick) Nothing

nickChange :: Identifier -> Identifier -> ChannelState -> ChannelState
nickChange fromNick toNick cs =
  set (chanUsers . at toNick) modes cs'
  where
  (modes, cs') = cs & chanUsers . at fromNick <<.~ Nothing

setTopic :: Text -> ChannelState -> ChannelState
setTopic = set chanTopic
