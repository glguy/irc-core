{-# Language OverloadedStrings, BangPatterns #-}
{-|
Module      : Client.Image.Focus
Description : Renderer for focus labels
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides image renderers used to construct
the labels for window names and activity boxes.
-}
module Client.Image.Focus
  ( FocusLabelType (..)
  , focusLabel
  , windowLabel
  ) where

import Client.Image.Message (cleanChar, cleanText, IdentifierColorMode (NormalIdentifier), coloredIdentifier, modesImage)
import Client.Image.PackedImage
import Client.Image.Palette
import Client.State
import Client.State.Channel (chanModes, chanUsers)
import Client.State.Focus (focusNetwork, Focus(..))
import Client.State.Network
import Client.State.Window
import Control.Lens (view, preview, Ixed(ix))
import Data.Map.Strict qualified as Map
import qualified Data.HashMap.Strict as HashMap
import Graphics.Vty.Attributes (defAttr)

windowLabel :: ClientState -> (Focus, Window) -> Image'
windowLabel st (focus, w) =
  jumpLabel <>
  focusLabel FocusLabelJump st focus <>
  activity
  where
    jumpLabel =
      case view winName w of
        Nothing   -> mempty
        Just name -> char (view palWindowName pal) name <>
                     char defAttr ':'
    n   = view winUnread w
    pal = clientPalette st
    activity
      | n == 0 = mempty
      | view winMention w == WLImportant = char defAttr ' ' <> string (view palMention pal) (show n)
      | otherwise = char defAttr ' ' <> string (view palActivity pal) (show n)

data FocusLabelType = FocusLabelJump | FocusLabelShort | FocusLabelLong

focusLabel :: FocusLabelType -> ClientState -> Focus -> Image'
focusLabel labelType st focus =
  let
    !pal = clientPalette st
    netpal = clientNetworkPalette st
    colon = char defAttr ':'
    networkLabel network = text' (view palLabel pal) (cleanText network)
    channelLabel         = coloredIdentifier pal NormalIdentifier HashMap.empty
  in case (focus, labelType) of
    (Unfocused, _) ->
      char (view palError pal) '*'
    (NetworkFocus network, FocusLabelJump) -> networkLabel network <> colon
    (NetworkFocus network, _) -> networkLabel network
    (ChannelFocus network channel, FocusLabelJump)
      | Just network == focusNetwork (view clientFocus st) -> channelLabel channel
    (ChannelFocus network channel, FocusLabelLong) ->
      networkLabel network <>
      colon <>
      string (view palSigil pal) (cleanChar <$> sigils) <>
      channelLabel channel <>
      channelModes
      where
        (sigils, channelModes) =
          case preview (clientConnection network) st of
            Just cs ->
               ( let nick = view csNick cs in
                 view (csChannels . ix channel . chanUsers . ix nick) cs
               , case preview (csChannels . ix channel . chanModes) cs of
                    Just modeMap | not (null modeMap) ->
                        " " <> modesImage (view palModes pal) (view palCModes netpal) ('+':Map.keys modeMap)
                    _ -> mempty
               )
            _ -> ("", mempty)
    (ChannelFocus network channel, _) ->
      networkLabel network <> colon <> channelLabel channel

