{-# Language BangPatterns #-}
{-|
Module      : Client.Image
Description : UI renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the renderer for the client's UI.

-}
module Client.Image (clientPicture) where

import           Client.Configuration
import           Client.ConnectionState
import qualified Client.EditBox as Edit
import           Client.Focus
import           Client.Image.ChannelInfo
import           Client.Image.MaskList
import           Client.Image.Message
import           Client.Image.MircFormatting
import           Client.Image.Palette
import           Client.Image.StatusLine
import           Client.Image.UserList
import           Client.Message
import           Client.State
import           Client.Window
import           Control.Lens
import           Data.Maybe
import qualified Data.Text as Text
import           Graphics.Vty (Picture(..), Cursor(..), picForImage)
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier)

-- | Generate a 'Picture' for the current client state. The resulting
-- client state is updated for render specific information like scrolling.
clientPicture :: ClientState -> (Picture, ClientState)
clientPicture st = (pic, st')
    where
      (pos, img, st') = clientImage st
      pic0 = picForImage img
      pic  = pic0 { picCursor = cursor }
      cursor = Cursor (min (view clientWidth st - 1) (pos+1))
                      (view clientHeight st - 1)

clientImage :: ClientState -> (Int, Image, ClientState)
clientImage st = (pos, img, st')
  where
    (mp, st') = messagePane st
    (pos, tbImg) = textboxImage st'
    img = vertCat
            [ mp
            , statusLineImage st'
            , tbImg
            ]

messagePaneImages :: ClientState -> [Image]
messagePaneImages !st =
  case (view clientFocus st, view clientSubfocus st) of
    (ChannelFocus network channel, FocusInfo) ->
      channelInfoImages network channel st
    (ChannelFocus network channel, FocusUsers)
      | view clientDetailView st -> userInfoImages network channel st
      | otherwise                -> userListImages network channel st
    (ChannelFocus network channel, FocusMasks mode) ->
      maskListImages mode network channel st

    -- subfocuses only make sense for channels
    _ -> chatMessageImages st

chatMessageImages :: ClientState -> [Image]
chatMessageImages st = windowLineProcessor focusedMessages
  where
    matcher = clientMatcher st

    focusedMessages
        = filter (views wlText matcher)
        $ view (clientWindows . ix (view clientFocus st) . winMessages) st

    windowLineProcessor
      | view clientDetailView st = map (view wlFullImage)
      | otherwise                = windowLinesToImages st . filter (not . isNoisy)

    isNoisy msg =
      case view wlBody msg of
        IrcBody irc -> squelchIrcMsg irc
        _           -> False

messagePane :: ClientState -> (Image, ClientState)
messagePane st = (img, st')
  where
    images = messagePaneImages st
    vimg = assemble emptyImage images
    vimg1 = cropBottom h vimg
    img   = pad 0 (h - imageHeight vimg1) 0 0 vimg1

    overscroll = vh - imageHeight vimg

    st' = over clientScroll (max 0 . subtract overscroll) st

    assemble acc _ | imageHeight acc >= vh = cropTop vh acc
    assemble acc [] = acc
    assemble acc (x:xs) = assemble (lineWrap w x <-> acc) xs

    scroll = view clientScroll st
    vh = h + scroll
    h = view clientHeight st - 2
    w = view clientWidth st

windowLinesToImages :: ClientState -> [WindowLine] -> [Image]
windowLinesToImages st wwls =
  case windowLinesToImagesMd st wwls of
    Just (img, _, wls) -> img : windowLinesToImages st wls
    Nothing ->
      case wwls of
        [] -> []
        wl:wls -> view wlImage wl : windowLinesToImages st wls

windowLinesToImagesMd ::
  ClientState -> [WindowLine] -> Maybe (Image, Identifier, [WindowLine])
windowLinesToImagesMd st wwls =
  do w:wls                <- Just wwls
     (img, ident, mbnext) <- metadataWindowLine st w
     let palette = view (clientConfig . configPalette) st

         (acc1, wls2) =
           case windowLinesToImagesMd st wls of
             Nothing -> (quietIdentifier palette ident <|> img, wls)
             Just (acc, prevident, wls') -> (acc <|> transition <|> img, wls')
               where
                 transition
                   | ident == prevident = emptyImage
                   | otherwise          = char defAttr ' ' <|> quietIdentifier palette ident

     let transition2 = foldMap (quietIdentifier palette) mbnext
     Just (acc1 <|> transition2, fromMaybe ident mbnext, wls2)

metadataWindowLine :: ClientState -> WindowLine -> Maybe (Image, Identifier, Maybe Identifier)
metadataWindowLine st wl =
  case view wlBody wl of
    IrcBody irc
      | Just who <- ircIgnorable irc st -> Just (ignoreImage, who, Nothing)
      | otherwise                       -> metadataImg irc
    _                                   -> Nothing

lineWrap :: Int -> Image -> Image
lineWrap w img
  | imageWidth img > w = cropRight w img <-> lineWrap w (cropLeft (imageWidth img - w) img)
  | otherwise = img <|> char defAttr ' '
                        -- trailing space with default attributes deals with bug in VTY
                        -- where the formatting will continue past the end of chat messages



textboxImage :: ClientState -> (Int, Image)
textboxImage st
  = (pos, applyCrop $ beginning <|> content <|> ending)
  where
  width = view clientWidth st
  (pos, content) = views (clientTextBox . Edit.content) renderContent st
  applyCrop
    | 1+pos < width = cropRight width
    | otherwise     = cropLeft  width . cropRight (pos+2)

  attr      = view (clientConfig . configPalette . palTextBox) st
  beginning = char attr '^'
  ending    = char attr '$'

renderContent :: Edit.Content -> (Int, Image)
renderContent c = (imgPos, wholeImg)
  where
  as = view Edit.above c
  bs = view Edit.below c
  cur = view Edit.current c

  imgPos = view Edit.pos cur + length as + sum (map length as)

  renderLine l = parseIrcTextExplicit $ Text.pack l

  curImg = views Edit.text renderLine cur
  rightImg = foldl (\i b -> i <|> renderLine ('\n':b)) curImg bs
  wholeImg = foldl (\i a -> renderLine (a ++ "\n") <|> i) rightImg as
