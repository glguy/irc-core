{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.Who
Description : Line renderer for /who replies
Copyright   : (c) TheDaemoness, 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in /who replies.
-}
module Client.View.Who ( whoLines ) where

import           Client.Image.LineWrap (lineWrapPrefix)
import           Client.Image.Message (IdentifierColorMode(NormalIdentifier), coloredIdentifier, coloredUserInfo, RenderMode (DetailedRender), prettyTime)
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Network
import           Client.WhoReply
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Attributes (defAttr)
import           Irc.Identifier
import qualified Data.HashMap.Strict as HashMap
import Client.Image.MircFormatting (parseIrcText')

-- |
-- | Render the lines used by the @/who@ command in normal mode.
whoLines ::
  Text        {- ^ network           -} ->
  Int         {- ^ window width      -} ->
  ClientState {- ^ client state      -} ->
  [Image']
whoLines network width st =
  -- TODO: This pattern exists in a few other views. Maybe deduplicate?
  case preview (clientConnection network) st of
    Just cs -> whoLines' cs width st
    Nothing -> [text' (view palError pal) "No connection"]
  where
    pal = clientPalette st

whoLines' :: NetworkState -> Int -> ClientState -> [Image']
whoLines' cs width st
  | Text.null $ view (csWhoReply . whoQuery . _1) cs = [text' (view palError pal) "No previous WHO query"]
  | whorpl^.whoDone = countImage <> queryPart : images
  | otherwise = countImagePending <> queryPart : images
  where
    pal = clientPalette st
    whorpl = view csWhoReply cs
    (query, arg) = view whoQuery whorpl
    entries = view whoItems whorpl
    entries' = clientFilter st whoFilterText entries

    images = concatMap renderEntry entries'
    renderEntry :: WhoReplyItem -> [Image']
    renderEntry entry = reverse $ lineWrapPrefix width (renderPrefix entry) (renderSuffix entry)
    -- Skipping rendering the channel because it doesn't add anything most of the time.
    renderPrefix entry = coloredUserInfo pal DetailedRender HashMap.empty (view whoUserInfo entry)
    renderSuffix :: WhoReplyItem -> Image'
    renderSuffix entry = mconcat $ mapJoinWhoFields entry
      (const mempty)
      (label "$a:" . identifier)
      (label "ip: " . text' defAttr)
      (label "server: " . identifier)
      (label "away" $ text' defAttr "")
      (label "flags: " . text' (view palSigil pal))
      (label "hops: " . string defAttr . show)
      (label "idle: " . string defAttr . prettyTime 1)
      (label "oplvl: " . text' defAttr)
      (label "gecos: " . parseIrcText' False pal)

    countImagePending = countImage <> text' (view palLabel pal) "..."
    countImage = text' (view palLabel pal) "Users in " <>
                 coloredIdentifier pal NormalIdentifier HashMap.empty (mkId query) <>
                 text' (view palLabel pal) " (visible/total): " <>
                 string defAttr (show (length entries')) <>
                 char (view palLabel pal) '/' <>
                 string defAttr (show (length entries))
    queryPart = case arg of
      Just txt | not (Text.null txt) -> label " Options: " $ text' defAttr txt
      _ -> text' defAttr ""

    label txt image = text' (view palLabel pal) txt <> image <> text' defAttr " "
    identifier = coloredIdentifier pal NormalIdentifier HashMap.empty
