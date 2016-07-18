{-# Language TemplateHaskell #-}
module Client.ChannelState where

import           Control.Lens
import           Data.HashMap.Strict
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.HashSet
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
  , _chanLists :: !(Map Char (HashSet Text))
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
  }

chanList :: Functor f => Char -> LensLike' f ChannelState (HashSet Text)
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
