{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.Messages
Description : Chat message view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module returns the chat messages for the currently focused
window in message view and gathers metadata entries into single
lines.

-}
module Client.View.Messages
  ( chatMessageImages
  ) where

import           Client.Configuration
import           Client.Image.LineWrap
import           Client.Image.Message
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.Message
import           Client.State
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Lens
import           Control.Monad
import           Data.List
import           Irc.Identifier
import           Irc.Message
import           Irc.UserInfo
import           ContextFilter (filterContext)


chatMessageImages :: Focus -> Int -> ClientState -> [Image']
chatMessageImages focus w st =
  case preview (clientWindows . ix focus) st of
    Nothing  -> []
    Just win ->
      let msgs     = toListOf each (view winMessages win)
          hideMeta = view winHideMeta win in
      case clientMatcher st of
        Just (Matcher a b p) -> windowLineProcessor hideMeta (filterContext b a (views wlText p) msgs)
        Nothing ->
          case view winMarker win of
            Nothing -> windowLineProcessor hideMeta msgs
            Just n  ->
              windowLineProcessor hideMeta l ++
              [marker] ++
              windowLineProcessor hideMeta r
              where
                (l,r) = splitAt n msgs

  where
    palette = clientPalette st
    marker = string (view palLineMarker palette) (replicate w '-')
    windowLineProcessor hideMeta
      | view clientDetailView st =
          concatMap (reverse . fullLineWrap w) .
          if hideMeta
            then detailedImagesWithoutMetadata st
            else map (view wlFullImage)

      | otherwise = windowLinesToImages st w hideMeta . filter (not . isNoisy)

    isNoisy msg =
      case view wlSummary msg of
        ReplySummary code -> squelchIrcMsg (Reply code [])
        _                 -> False

detailedImagesWithoutMetadata :: ClientState -> [WindowLine] -> [Image']
detailedImagesWithoutMetadata st wwls =
  case gatherMetadataLines st wwls of
    ([], [])   -> []
    ([], w:ws) -> view wlFullImage w
                : detailedImagesWithoutMetadata st ws
    (_:_, wls) -> detailedImagesWithoutMetadata st wls


windowLinesToImages ::
  ClientState  {- ^ client state  -} ->
  Int          {- ^ draw width    -} ->
  Bool         {- ^ hide metadata -} ->
  [WindowLine] {- ^ window lines  -} ->
  [Image']     {- ^ image lines   -}
windowLinesToImages st w hideMeta wwls =
  case gatherMetadataLines st wwls of
    ([], [])   -> []
    ([], wl:wls) -> drawWindowLine palette w padAmt wl
                ++ windowLinesToImages st w hideMeta wls
    ((img,who,mbnext):mds, wls)

      | hideMeta -> windowLinesToImages st w hideMeta wls

      | otherwise ->
         wrap
           metaPad
           (mconcat
              (intersperse " "
                 (startMetadata img mbnext who mds palette)))
      ++ windowLinesToImages st w hideMeta wls

  where
    palette = clientPalette st
    config  = view clientConfig st
    padAmt  = view configNickPadding config

    padNick  = nickPad padAmt
    metaPad  = "      " <> padNick ""

    wrap pfx body = reverse (lineWrapPrefix w pfx body)

------------------------------------------------------------------------

type MetadataState =
  Identifier                             {- ^ current nick -} ->
  [(Image',Identifier,Maybe Identifier)] {- ^ metadata     -} ->
  Palette                                {- ^ palette      -} ->
  [Image']

startMetadata ::
  Image'           {- ^ metadata image           -} ->
  Maybe Identifier {- ^ possible nick transition -} ->
  MetadataState
startMetadata img mbnext who mds palette =
  let acc = quietIdentifier palette who <> img
  in transitionMetadata acc mbnext who mds palette

transitionMetadata ::
  Image'           {- ^ image accumulator        -} ->
  Maybe Identifier {- ^ possible nick transition -} ->
  MetadataState
transitionMetadata acc mbwho who mds palette =
  case mbwho of
    Nothing   -> continueMetadata acc who mds palette
    Just who' ->
      let acc' = acc <> quietIdentifier palette who'
      in continueMetadata acc' who' mds palette

continueMetadata :: Image' -> MetadataState
continueMetadata acc _ [] _ = [acc]
continueMetadata acc who1 ((img, who2, mbwho3):mds) palette
  | who1 == who2 = let acc' = acc <> img
                   in transitionMetadata acc' mbwho3 who2 mds palette
  | otherwise    = acc : startMetadata img mbwho3 who2 mds palette

------------------------------------------------------------------------

gatherMetadataLines ::
  ClientState ->
  [WindowLine] ->
  ( [(Image', Identifier, Maybe Identifier)] , [ WindowLine ] )
  -- ^ metadata entries are reversed
gatherMetadataLines st = go []
  where
    go acc (w:ws)
      | Just (img,who,mbnext) <- metadataWindowLine st w =
          go ((img,who,mbnext) : acc) ws

    go acc ws = (acc,ws)


-- | Classify window lines for metadata coalesence
metadataWindowLine ::
  ClientState ->
  WindowLine ->
  Maybe (Image', Identifier, Maybe Identifier)
        {- ^ Image, incoming identifier, outgoing identifier if changed -}
metadataWindowLine st wl =
  case view wlSummary wl of
    ChatSummary who -> (ignoreImage, userNick who, Nothing) <$ guard (identIgnored who st)
    summary         -> metadataImg summary
