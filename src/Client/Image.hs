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
import           Client.ConnectionState
import qualified Client.EditBox as Edit
import           Client.Image.ChannelInfo
import           Client.Image.MaskList
import           Client.Image.Message
import           Client.Image.UserList
import           Client.Message
import           Client.MircFormatting
import           Client.State
import           Client.Window
import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
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
      (img, st') = clientImage st
      pic0 = picForImage img
      pic  = pic0 { picCursor = cursor }
      cursor = Cursor (min (view clientWidth st - 1)
                           (view (clientTextBox . Edit.pos) st+1))
                      (view clientHeight st - 1)

clientImage :: ClientState -> (Image, ClientState)
clientImage st = (img, st')
  where
    (mp, st') = messagePane st
    img = vertCat
            [ mp
            , horizDividerImage st'
            , textboxImage st'
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
  case wwls of
    [] -> []
    wl:wls
      | Just (img,ident) <- metadataWindowLine st wl -> windowLinesToImagesMd st img ident wls
      | otherwise -> view wlImage wl : windowLinesToImages st wls

windowLinesToImagesMd :: ClientState -> Image -> Maybe Identifier -> [WindowLine] -> [Image]
windowLinesToImagesMd st acc who wwls =
  case wwls of
    wl:wls
      | Just (img,ident) <- metadataWindowLine st wl ->
          if isJust ident && who == ident
            then windowLinesToImagesMd st (acc <|> img) who wls
            else windowLinesToImagesMd st (finish <|> char defAttr ' ' <|> img) ident wls
    _ -> finish : windowLinesToImages st wwls
  where
    finish = acc <|> maybe emptyImage quietIdentifier who


metadataWindowLine :: ClientState -> WindowLine -> Maybe (Image, Maybe Identifier)
metadataWindowLine st wl =
  case view wlBody wl of
    IrcBody irc
      | Just who <- ircIgnorable irc st -> Just (ignoreImage, Just who)
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
      , string (withForeColor defAttr red) "scroll"
      , string defAttr ")"
      ]

detailImage :: ClientState -> Image
detailImage st
  | view clientDetailView st = horizCat
      [ string defAttr "─("
      , string (withForeColor defAttr red) "detail"
      , string defAttr ")"
      ]
  | otherwise = emptyImage

activityImage :: ClientState -> Image
activityImage st
  | null indicators = emptyImage
  | otherwise       = string defAttr "─[" <|>
                      horizCat indicators <|>
                      string defAttr "]"
  where
    windows = views clientWindows Map.elems st
    winNames = windowNames ++ repeat '?'
    indicators = aux (zip winNames windows)
    aux [] = []
    aux ((i,w):ws)
      | view winUnread w == 0 = aux ws
      | otherwise = char (withForeColor defAttr color) i : aux ws
      where
        color | view winMention w = red
              | otherwise        = green


myNickImage :: ClientState -> Image
myNickImage st =
  case view clientFocus st of
    NetworkFocus network      -> nickPart network Nothing
    ChannelFocus network chan -> nickPart network (Just chan)
    Unfocused                 -> emptyImage
  where
    nickPart network mbChan =
      case preview (clientConnection network) st of
        Nothing -> emptyImage
        Just cs -> string (withForeColor defAttr cyan) myChanModes
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
      [ char (withForeColor defAttr cyan) windowName
      , char defAttr ':'
      , renderedFocus
      ]

    focus = view clientFocus st
    windowName =
      case Map.lookupIndex focus (view clientWindows st) of
        Nothing -> '?'
        Just i  -> (windowNames ++ repeat '?') !! i

    subfocusName =
      case view clientSubfocus st of
        FocusMessages -> Nothing
        FocusInfo     -> Just $ string (withForeColor defAttr green) "info"
        FocusUsers    -> Just $ string (withForeColor defAttr green) "users"
        FocusMasks m  -> Just $ horizCat
          [ string (withForeColor defAttr green) "masks"
          , char defAttr ':'
          , char (withForeColor defAttr green) m
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
          char (withForeColor defAttr red) '*'
        NetworkFocus network ->
          text' (withForeColor defAttr green) network
        ChannelFocus network channel ->
          text' (withForeColor defAttr green) network <|>
          char defAttr ':' <|>
          text' (withForeColor defAttr green) (idText channel) <|>
          channelModesImage network channel st

channelModesImage :: Text -> Identifier -> ClientState -> Image
channelModesImage network channel st =
  case preview (clientConnection network . csChannels . ix channel . chanModes) st of
    Just modeMap | not (null modeMap) ->
        string defAttr (" +" ++ modes) <|>
        horizCat [ char defAttr ' ' <|> text' defAttr arg | arg <- args, not (Text.null arg) ]
      where (modes,args) = unzip (Map.toList modeMap)
    _ -> emptyImage

textboxImage :: ClientState -> Image
textboxImage st
  = applyCrop
  $ beginning <|> content <|> ending
  where
  pos = view (clientTextBox . Edit.pos) st
  width = view clientWidth st
  content = parseIrcTextExplicit (Text.pack (view (clientTextBox . Edit.content) st))
  applyCrop
    | 1+pos < width = cropRight width
    | otherwise     = cropLeft  width . cropRight (pos+2)

  beginning = char (withForeColor defAttr brightBlack) '^'
  ending    = char (withForeColor defAttr brightBlack) '$'

latencyImage :: ClientState -> Image
latencyImage st
  | Just network <- views clientFocus focusNetwork st
  , Just cs      <- preview (clientConnection network) st =
  case view csPingStatus cs of
    PingNever -> emptyImage
    PingSent {} -> emptyImage
    PingLatency delta -> horizCat
      [ string defAttr "─("
      , string (withForeColor defAttr yellow) (showFFloat (Just 2) delta "s")
      , string defAttr ")"
      ]
  | otherwise = emptyImage
