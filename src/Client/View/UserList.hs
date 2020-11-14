{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.UserList
Description : Line renderers for channel user list view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel user list.
-}
module Client.View.UserList
  ( userListImages
  , userInfoImages
  ) where

import           Client.Image.Message
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Channel
import           Client.State.Focus
import           Client.State.Network
import           Client.UserHost
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import           Graphics.Vty.Attributes
import           Irc.Identifier
import           Irc.UserInfo

-- | Render the lines used by the @/users@ command in normal mode.
-- These lines show the count of users having each channel mode
-- in addition to the nicknames of the users.
userListImages ::
  Text        {- ^ network              -} ->
  Identifier  {- ^ channel              -} ->
  Int         {- ^ window width         -} ->
  ClientState {- ^ client state         -} ->
  [Image']
userListImages network channel w st =
  case preview (clientConnection network) st of
    Just cs -> userListImages' cs channel w st
    Nothing -> [text' (view palError pal) "No connection"]
  where
    pal = clientPalette st

userListImages' :: NetworkState -> Identifier -> Int -> ClientState -> [Image']
userListImages' cs channel w st
  = countImage : reverse [mconcat (intersperse gap row) | row <- chunksOf columns paddedNames]
  where
    paddedNames = map (resizeImage maxWidth) nameImages
    nameImages = map renderUser usersList
    maxWidth   = maximum (map imageWidth nameImages)
    columns    = max 1 ((w+1) `quot` (maxWidth+1))

    countImage = drawSigilCount pal (map snd usersList)

    hilites = clientHighlightsFocus (ChannelFocus (view csNetwork cs) channel) st

    renderUser (ident, sigils) =
      string (view palSigil pal) sigils <>
      coloredIdentifier pal NormalIdentifier hilites ident

    gap = char defAttr ' '

    filterOn (ident,sigils) = LText.fromChunks [Text.pack sigils, idText ident]

    usersList = sortBy (comparing fst)
              $ clientFilter st filterOn
              $ HashMap.toList usersHashMap

    pal = clientPalette st

    usersHashMap =
      view (csChannels . ix channel . chanUsers) cs

drawSigilCount :: Palette -> [String] -> Image'
drawSigilCount pal sigils =
  text' (view palLabel pal) "Users:" <> mconcat entries
  where
    sigilCounts = Map.fromListWith (+) [ (take 1 sigil, 1::Int) | sigil <- sigils ]

    entries
      | Map.null sigilCounts = [" 0"]
      | otherwise = [ string (view palSigil pal) (' ':sigil) <>
                      string defAttr (show n)
                    | (sigil,n) <- Map.toList sigilCounts
                    ]


-- | Render lines for the @/users@ command in detailed view.
-- Each user will be rendered on a separate line with username
-- and host visible when known.
userInfoImages ::
  Text        {- ^ network -} ->
  Identifier  {- ^ channel -} ->
  ClientState                 ->
  [Image']
userInfoImages network channel st =
  case preview (clientConnection network) st of
    Just cs -> userInfoImages' cs channel st
    Nothing -> [text' (view palError pal) "No connection"]
  where
    pal = clientPalette st

userInfoImages' :: NetworkState -> Identifier -> ClientState -> [Image']
userInfoImages' cs channel st = countImage : map renderEntry usersList
  where
    countImage = drawSigilCount pal (map snd usersList)

    hilites = clientHighlightsFocus (ChannelFocus (view csNetwork cs) channel) st

    pal = clientPalette st

    renderEntry ((info, acct), sigils) =
      string (view palSigil pal) sigils <>
      coloredUserInfo pal DetailedRender hilites info <>
      " " <> text' (view palMeta pal) (cleanText acct)

    filterOn ((info, acct),sigils) =
      LText.fromChunks [Text.pack sigils, renderUserInfo info, " ", acct]

    userInfos = view csUsers cs

    toInfo nick =
      case view (at nick) userInfos of
        Just (UserAndHost n h a) -> (UserInfo nick n h, a)
        Nothing                  -> (UserInfo nick "" "", "")

    usersList = sortBy (flip (comparing (userNick . fst . fst)))
              $ clientFilter st filterOn
              $ map (over _1 toInfo)
              $ HashMap.toList usersHashMap

    usersHashMap = view (csChannels . ix channel . chanUsers) cs
