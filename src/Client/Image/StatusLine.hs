{-|
Module      : Client.Image.StatusLine
Description : Renderer for status line
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides image renderers used to construct
the status image that sits between text input and the message
window.

-}
module Client.Image.StatusLine
  ( statusLineImage
  ) where

import           Client.Configuration
import           Client.Image.Palette
import           Client.State
import           Client.State.Channel
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier, idText)
import           Numeric

statusLineImage :: ClientState -> Image
statusLineImage st
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
    PingConnecting n _ -> horizCat . fmap (string defAttr) $
      [ "─(Connecting" ] ++
      (if n > 0
          then [": ", show n, "retr", if n == 1 then "y" else "ies"]
          else []) ++
      [ ")" ]
  | otherwise = emptyImage
  where
    pal = view (clientConfig . configPalette) st

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
        FocusWindows  -> Just $ string (view palLabel pal) "windows"
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

parens :: Attr -> Image -> Image
parens attr i = char attr '(' <|> i <|> char attr ')'
