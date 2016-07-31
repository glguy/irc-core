{-# Language OverloadedStrings #-}
{-|
Module      : Client.Image.UserList
Description : Line renderers for channel user list view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel user list.
-}
module Client.Image.UserList where

import           Client.ChannelState
import           Client.ConnectionState
import           Client.Image.Message
import           Client.State
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Ord
import qualified Data.Text as Text
import           Graphics.Vty.Image
import           Irc.Identifier
import           Irc.UserInfo

-- | Render the lines used in a simple user list window.
userListImages ::
  NetworkName {- ^ Focused network name -} ->
  Identifier  {- ^ Focused channel name -} ->
  ClientState -> [Image]
userListImages network channel st =
    [countImage, horizCat (intersperse gap (map renderUser usersList))]
  where
    countImage = text' (withForeColor defAttr green) "Users: " <|>
                 string defAttr (show (HashMap.size usersHashMap))

    matcher = clientMatcher st

    myNicks = toListOf (clientConnection network . csNick) st

    renderUser (ident, sigils) =
      string (withForeColor defAttr cyan) sigils <|>
      coloredIdentifier myNicks ident

    gap = char defAttr ' '

    matcher' (ident,sigils) = matcher (Text.pack sigils `Text.append` idText ident)

    usersList = sortBy (comparing fst)
              $ filter matcher'
              $ HashMap.toList usersHashMap

    usersHashMap =
      view ( clientConnection network
           . csChannels . ix channel
           . chanUsers ) st

-- | Render lines for detailed channel user list which shows full user info.
userInfoImages ::
  NetworkName {- ^ Focused network name -} ->
  Identifier  {- ^ Focused channel name -} ->
  ClientState -> [Image]
userInfoImages network channel st = renderEntry <$> usersList
  where
    matcher = clientMatcher st

    myNicks = toListOf (clientConnection network . csNick) st

    renderEntry (info, sigils) =
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo DetailedRender myNicks info

    matcher' (info,sigils) =
      matcher (Text.pack sigils `Text.append` renderUserInfo info)

    userInfos = view (clientConnection network . csUsers) st

    toInfo nick =
      case view (at nick) userInfos of
        Just (UserAndHost n h) -> UserInfo nick (Just n) (Just h)
        Nothing                -> UserInfo nick Nothing Nothing

    usersList = sortBy (flip (comparing (userNick . fst)))
              $ filter matcher'
              $ map (over _1 toInfo)
              $ HashMap.toList usersHashMap

    usersHashMap =
      view ( clientConnection network
           . csChannels . ix channel
           . chanUsers ) st
