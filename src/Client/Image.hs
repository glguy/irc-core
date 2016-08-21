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
import           Client.Image.ChannelInfo
import           Client.Image.MaskList
import           Client.Image.Message
import           Client.Image.MircFormatting
import           Client.Image.Palette
import           Client.Image.StatusLine
import           Client.Image.UserList
import           Client.Message
import           Client.State
import qualified Client.State.EditBox as Edit
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Lens
import           Data.Char
import           Data.List
import qualified Data.Text as Text
import           Graphics.Vty (Background(..), Picture(..), Cursor(..))
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier)

-- | Generate a 'Picture' for the current client state. The resulting
-- client state is updated for render specific information like scrolling.
clientPicture :: ClientState -> (Picture, ClientState)
clientPicture st = (pic, st')
    where
      (pos, img, st') = clientImage st
      pic = Picture
              { picCursor     = Cursor pos (view clientHeight st - 1)
              , picBackground = ClearBackground
              , picLayers     = [img]
              }

clientImage ::
  ClientState ->
  (Int, Image, ClientState) -- ^ text box cursor position, image, updated state
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
    vimg   = assemble emptyImage images
    vimg1  = cropBottom h vimg
    img    = pad 0 (h - imageHeight vimg1) 0 0 vimg1

    overscroll = vh - imageHeight vimg

    st' = over clientScroll (max 0 . subtract overscroll) st

    assemble acc _ | imageHeight acc >= vh = cropTop vh acc
    assemble acc [] = acc
    assemble acc (x:xs) = assemble (lineWrap w x <-> acc) xs

    scroll = view clientScroll st
    vh     = h + scroll
    h      = view clientHeight st - 2
    w      = view clientWidth st

windowLinesToImages :: ClientState -> [WindowLine] -> [Image]
windowLinesToImages st wwls =
  case gatherMetadataLines st wwls of
    ([], [])   -> []
    ([], w:ws) -> view wlImage w : windowLinesToImages st ws
    ((img,who,mbnext):mds, wls) ->
         startMetadata img mbnext who mds palette
       : windowLinesToImages st wls
  where
    palette = view (clientConfig . configPalette) st

------------------------------------------------------------------------

type MetadataState =
  Identifier                            {- ^ current nick -} ->
  [(Image,Identifier,Maybe Identifier)] {- ^ metadata     -} ->
  Palette                               {- ^ palette      -} ->
  Image

startMetadata ::
  Image            {- ^ metadata image           -} ->
  Maybe Identifier {- ^ possible nick transition -} ->
  MetadataState
startMetadata img mbnext who mds palette =
        quietIdentifier palette who
    <|> img
    <|> transitionMetadata mbnext who mds palette

transitionMetadata ::
  Maybe Identifier {- ^ possible nick transition -} ->
  MetadataState
transitionMetadata mbwho who mds palette =
  case mbwho of
    Nothing   -> continueMetadata who  mds palette
    Just who' -> quietIdentifier palette who'
             <|> continueMetadata who' mds palette

continueMetadata :: MetadataState
continueMetadata _ [] _ = emptyImage
continueMetadata who1 ((img, who2, mbwho3):mds) palette
  | who1 == who2 = img
               <|> transitionMetadata mbwho3 who2 mds palette
  | otherwise    = char defAttr ' '
               <|> startMetadata img mbwho3 who2 mds palette

------------------------------------------------------------------------

gatherMetadataLines ::
  ClientState ->
  [WindowLine] ->
  ( [(Image, Identifier, Maybe Identifier)] , [ WindowLine ] )
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
  Maybe (Image, Identifier, Maybe Identifier)
        {- ^ Image, incoming identifier, outgoing identifier if changed -}
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
  = (pos, croppedImage)
  where
  width = view clientWidth st
  (txt, content) =
     views (clientTextBox . Edit.content) renderContent st

  pos = computeCharWidth (width-1) txt

  lineImage = beginning <|> content <|> ending

  leftOfCurWidth = safeWcswidth txt

  croppedImage
    | leftOfCurWidth < width = lineImage
    | otherwise = cropLeft width (cropRight (leftOfCurWidth+1) lineImage)

  attr      = view (clientConfig . configPalette . palTextBox) st
  beginning = char attr '^'
  ending    = char attr '$'

renderContent :: Edit.Content -> (String, Image)
renderContent c = (txt, wholeImg)
  where
  as  = reverse (view Edit.above c)
  bs  = view Edit.below c
  cur = view Edit.line c

  leftCur = take (view Edit.pos cur) (view Edit.text cur)

  renderLine l = parseIrcTextExplicit $ Text.pack l

  inputLines = as ++ view Edit.text cur : bs

  -- ["one","two"] "three" --> "^two one three"
  txt = '^' : foldl (\acc x -> x++' ':acc) leftCur as

  wholeImg = horizCat
           $ intersperse (renderLine "\n")
           $ map renderLine inputLines

computeCharWidth :: Int -> String -> Int
computeCharWidth = go 0
  where
    go !acc _ [] = acc
    go acc 0 _ = acc
    go acc w (x:xs)
      | z > w = acc + w -- didn't fit, will be filled in
      | otherwise = go (acc+1) (w-z) xs
      where
        z | isControl x = 1
          | otherwise   = safeWcwidth x
