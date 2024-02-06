{-# Language TemplateHaskell #-}

{-|
Module      : Client.State.Channel
Description : IRC channel session state
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module is responsible for tracking the state of an individual IRC
channel while the client is connected to it. When the client joins a
channel a new channel session is created and when the client leaves
a channel is it destroyed.
-}

module Client.State.Channel
  (
  -- * Channel state
    ChannelState(..)
  , chanStale
  , chanTopic
  , chanTopicProvenance
  , chanUrl
  , chanUsers
  , chanModes
  , chanLists
  , chanCreation
  , chanQueuedModeration

  -- * Mask list entries
  , MaskListEntry(..)
  , maskListSetter
  , maskListTime

  -- * Topic information
  , TopicProvenance(..)
  , topicAuthor
  , topicTime

  -- * Channel manipulation
  , newChannel
  , setTopic
  , joinChannel
  , partChannel
  , nickChange
  ) where

import Control.Lens ((&), sans, (<<.~), over, set, makeLenses, At(at))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Irc.Identifier (Identifier)
import Irc.RawIrcMsg (RawIrcMsg)
import Irc.UserInfo (UserInfo)

-- | Dynamic information about the state of an IRC channel
data ChannelState = ChannelState
  { _chanStale :: Bool
        -- ^ whether the channel state may be stale because we're not actually in it
  , _chanTopic :: !Text
        -- ^ topic text
  , _chanTopicProvenance :: !(Maybe TopicProvenance)
        -- ^ author and timestamp for topic
  , _chanUrl :: !(Maybe Text)
        -- ^ channel URL
  , _chanUsers :: !(HashMap Identifier String)
        -- ^ user list and sigils
  , _chanModes :: !(Map Char Text)
        -- ^ channel settings and parameters
  , _chanLists :: !(Map Char (HashMap Text MaskListEntry))
        -- ^ mode, mask, setter, set time
  , _chanCreation :: !(Maybe UTCTime) -- ^ creation time of channel
  , _chanQueuedModeration :: ![RawIrcMsg] -- ^ delayed op messages
  }
  deriving Show

data TopicProvenance = TopicProvenance
  { _topicAuthor :: !UserInfo
  , _topicTime   :: !UTCTime
  }
  deriving Show

data MaskListEntry = MaskListEntry
  { _maskListSetter :: {-# UNPACK #-} !Text
  , _maskListTime   :: {-# UNPACK #-} !UTCTime
  }
  deriving Show

makeLenses ''ChannelState
makeLenses ''TopicProvenance
makeLenses ''MaskListEntry

-- | Construct an empty 'ChannelState'
newChannel :: Bool -> ChannelState
newChannel stale = ChannelState
  { _chanStale = stale
  , _chanTopic = Text.empty
  , _chanUrl = Nothing
  , _chanTopicProvenance = Nothing
  , _chanUsers = HashMap.empty
  , _chanModes = Map.empty
  , _chanLists = Map.empty
  , _chanCreation = Nothing
  , _chanQueuedModeration = []
  }


-- | Add a user to the user list
joinChannel :: Identifier -> ChannelState -> ChannelState
joinChannel nick = set (chanUsers . at nick) (Just "")

-- | Remove a user from the user list
partChannel :: Identifier -> ChannelState -> ChannelState
partChannel = over chanUsers . sans

-- | Rename a user in the user list
nickChange :: Identifier -> Identifier -> ChannelState -> ChannelState
nickChange fromNick toNick cs =
  set (chanUsers . at toNick) modes cs'
  where
  (modes, cs') = cs & chanUsers . at fromNick <<.~ Nothing

-- | Set the channel topic
setTopic :: Text -> ChannelState -> ChannelState
setTopic = set chanTopic
