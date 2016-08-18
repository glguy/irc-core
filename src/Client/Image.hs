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

import           Client.ChannelState
import           Client.Configuration
import           Client.ConnectionState
import qualified Client.EditBox as Edit
import           Client.Focus
import           Client.Image.ChannelInfo
import           Client.Image.MaskList
import           Client.Image.Message
import           Client.Image.Palette
import           Client.Image.UserList
import           Client.Message
import           Client.MircFormatting
import           Client.State
import           Client.Window
import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty (Picture(..), Cursor(..), picForImage)
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier, idText)
import           Numeric

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
            , horizDividerImage st'
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


horizDividerImage :: ClientState -> Image
horizDividerImage st
  = content <|> charFill defAttr '─' fillSize 1
  where
    fillSize = max 0 (view clientWidth st - imageWidth content)
    content = horizCat
      [ myNickImage st
      , focusImage st
      , activityImage st
      , detailImage st
      , scrollImage st
      , latencyImage st
      ]

parens :: Attr -> Image -> Image
parens attr i = char attr '(' <|> i <|> char attr ')'

scrollImage :: ClientState -> Image
scrollImage st
  | 0 == view clientScroll st = emptyImage
  | otherwise = horizCat
      [ string defAttr "─("
      , string attr "scroll"
      , string defAttr ")"
      ]
  where
    attr = view (clientConfig . configPalette . palLabel) st

detailImage :: ClientState -> Image
detailImage st
  | view clientDetailView st = horizCat
      [ string defAttr "─("
      , string attr "detail"
      , string defAttr ")"
      ]
  | otherwise = emptyImage
  where
    attr = view (clientConfig . configPalette . palLabel) st

activityImage :: ClientState -> Image
activityImage st
  | null indicators = emptyImage
  | otherwise       = string defAttr "─[" <|>
                      horizCat indicators <|>
                      string defAttr "]"
  where
    windows = views clientWindows Map.elems st
    windowNames = view (clientConfig . configWindowNames) st
    winNames = Text.unpack windowNames ++ repeat '?'
    indicators = aux (zip winNames windows)
    aux [] = []
    aux ((i,w):ws)
      | view winUnread w == 0 = aux ws
      | otherwise = char attr i : aux ws
      where
        pal = view (clientConfig . configPalette) st
        attr | view winMention w = view palMention pal
             | otherwise         = view palActivity pal


myNickImage :: ClientState -> Image
myNickImage st =
  case view clientFocus st of
    NetworkFocus network      -> nickPart network Nothing
    ChannelFocus network chan -> nickPart network (Just chan)
    Unfocused                 -> emptyImage
  where
    pal = view (clientConfig . configPalette) st
    nickPart network mbChan =
      case preview (clientConnection network) st of
        Nothing -> emptyImage
        Just cs -> string (view palSigil pal) myChanModes
               <|> text' defAttr (idText nick)
               <|> parens defAttr (string defAttr ('+' : view csModes cs))
               <|> char defAttr '─'
          where
            nick      = view csNick cs
            myChanModes =
              case mbChan of
                Nothing   -> []
                Just chan -> view (csChannels . ix chan . chanUsers . ix nick) cs


focusImage :: ClientState -> Image
focusImage st = parens defAttr majorImage <|> renderedSubfocus
  where
    majorImage = horizCat
      [ char (view palWindowName pal) windowName
      , char defAttr ':'
      , renderedFocus
      ]

    pal = view (clientConfig . configPalette) st
    focus = view clientFocus st
    windowNames = view (clientConfig . configWindowNames) st

    windowName =
      case Map.lookupIndex focus (view clientWindows st) of
        Just i | i < Text.length windowNames -> Text.index windowNames i
        _ -> '?'

    subfocusName =
      case view clientSubfocus st of
        FocusMessages -> Nothing
        FocusInfo     -> Just $ string (view palLabel pal) "info"
        FocusUsers    -> Just $ string (view palLabel pal) "users"
        FocusMasks m  -> Just $ horizCat
          [ string (view palLabel pal) "masks"
          , char defAttr ':'
          , char (view palLabel pal) m
          ]

    renderedSubfocus =
      foldMap (\name -> horizCat
          [ string defAttr "─("
          , name
          , char defAttr ')'
          ]) subfocusName

    renderedFocus =
      case focus of
        Unfocused ->
          char (view palError pal) '*'
        NetworkFocus network ->
          text' (view palLabel pal) network
        ChannelFocus network channel ->
          text' (view palLabel pal) network <|>
          char defAttr ':' <|>
          text' (view palLabel pal) (idText channel) <|>
          channelModesImage network channel st

channelModesImage :: Text -> Identifier -> ClientState -> Image
channelModesImage network channel st =
  case preview (clientConnection network . csChannels . ix channel . chanModes) st of
    Just modeMap | not (null modeMap) ->
        string defAttr (" +" ++ modes) <|>
        horizCat [ char defAttr ' ' <|> text' defAttr arg | arg <- args, not (Text.null arg) ]
      where (modes,args) = unzip (Map.toList modeMap)
    _ -> emptyImage

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

latencyImage :: ClientState -> Image
latencyImage st
  | Just network <- views clientFocus focusNetwork st
  , Just cs      <- preview (clientConnection network) st =
  case view csPingStatus cs of
    PingNever -> emptyImage
    PingSent {} -> emptyImage
    PingLatency delta -> horizCat
      [ string defAttr "─("
      , string (view palLatency pal) (showFFloat (Just 2) delta "s")
      , string defAttr ")"
      ]
  | otherwise = emptyImage
  where
    pal = view (clientConfig . configPalette) st
