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
import           Client.Configuration
import           Client.ConnectionState
import           Client.Image.Message
import           Client.Image.Palette
import           Client.State
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
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
  case preview (clientConnection network) st of
    Just cs -> userListImages' cs channel st
    Nothing -> [text' (view palError pal) "No connection"]
  where
    pal = view (clientConfig . configPalette) st

userListImages' :: ConnectionState -> Identifier -> ClientState -> [Image]
userListImages' cs channel st =
    [countImage, horizCat (intersperse gap (map renderUser usersList))]
  where
    countImage = text' (view palLabel pal) "Users:" <|>
                 sigilCountImage

    matcher = clientMatcher st

    myNicks = toListOf csNick cs

    renderUser (ident, sigils) =
      string (view palSigil pal) sigils <|>
      coloredIdentifier pal NormalIdentifier myNicks ident

    gap = char defAttr ' '

    matcher' (ident,sigils) = matcher (Text.pack sigils `Text.append` idText ident)

    usersList = sortBy (comparing fst)
              $ filter matcher'
              $ HashMap.toList usersHashMap

    sigilCounts = Map.fromListWith (+)
                    [ (take 1 sigil, 1::Int) | (_,sigil) <- usersList ]

    sigilCountImage = horizCat
      [ string (view palSigil pal) (' ':sigil) <|>
        string defAttr (show n)
      | (sigil,n) <- Map.toList sigilCounts
      ]

    pal = view (clientConfig . configPalette) st

    usersHashMap =
      view (csChannels . ix channel . chanUsers) cs

-- | Render lines for detailed channel user list which shows full user info.
userInfoImages ::
  NetworkName {- ^ Focused network name -} ->
  Identifier  {- ^ Focused channel name -} ->
  ClientState -> [Image]
userInfoImages network channel st =
  case preview (clientConnection network) st of
    Just cs -> userInfoImages' cs channel st
    Nothing -> [text' (view palError pal) "No connection"]
  where
    pal = view (clientConfig . configPalette) st

userInfoImages' :: ConnectionState -> Identifier -> ClientState -> [Image]
userInfoImages' cs channel st = renderEntry <$> usersList
  where
    matcher = clientMatcher st

    myNicks = toListOf csNick cs

    pal = view (clientConfig . configPalette) st

    renderEntry (info, sigils) =
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal DetailedRender myNicks info

    matcher' (info,sigils) =
      matcher (Text.pack sigils `Text.append` renderUserInfo info)

    userInfos = view csUsers cs

    toInfo nick =
      case view (at nick) userInfos of
        Just (UserAndHost n h) -> UserInfo nick n h
        Nothing                -> UserInfo nick "" ""

    usersList = sortBy (flip (comparing (userNick . fst)))
              $ filter matcher'
              $ map (over _1 toInfo)
              $ HashMap.toList usersHashMap

    usersHashMap = view (csChannels . ix channel . chanUsers) cs
