module Client.Image.UserList where

import           Client.MessageRenderer
import           Client.NetworkConnection
import           Client.State
import           Client.ChannelState
import           Client.ConnectionState
import           Control.Lens
import           Irc.Identifier
import           Irc.UserInfo
import           Data.List
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import           Graphics.Vty.Image

userListImages ::
  (Text -> Bool) -> NetworkName -> Identifier -> ClientState -> [Image]
userListImages matcher network channel st =
    [horizCat (intersperse gap (map renderUser usersList))]
  where
    renderUser (ident, sigils) =
      string (withForeColor defAttr cyan) sigils <|>
      text' defAttr (idText ident)

    gap = char defAttr ' '

    matcher' (ident,sigils) = matcher (Text.pack sigils `Text.append` idText ident)

    usersList = sortBy (comparing fst)
              $ filter matcher'
              $ HashMap.toList usersHashMap

    usersHashMap =
      view ( clientConnections . ix network
           . csChannels        . ix channel
           . chanUsers ) st

-- | Detailed channel user list, shows full user info
userInfoImages ::
  (Text -> Bool) -> NetworkName -> Identifier -> ClientState -> [Image]
userInfoImages matcher network channel st =
  map renderEntry (reverse usersList)
  where
    renderEntry (info, sigils) =
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo DetailedRender info

    matcher' (info,sigils) =
      matcher (Text.pack sigils `Text.append` renderUserInfo info)

    userInfos = view (clientConnections . ix network . csUsers) st

    toInfo nick =
      case view (at nick . non (Nothing,Nothing)) userInfos of
        (n,h) -> UserInfo nick n h

    usersList = sortBy (flip (comparing (userNick . fst)))
              $ filter matcher'
              $ map (over _1 toInfo)
              $ HashMap.toList usersHashMap

    usersHashMap =
      view ( clientConnections . ix network
           . csChannels        . ix channel
           . chanUsers ) st
