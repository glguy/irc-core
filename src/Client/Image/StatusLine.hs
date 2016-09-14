{-# Language BangPatterns #-}
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
  , minorStatusLineImage
  , activityBarImage
  ) where

import           Client.Image.Palette
import           Client.State
import           Client.State.Channel
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier, idText)
import           Numeric

-- | Renders the status line between messages and the textbox.
statusLineImage ::
  ClientState {- ^ client state             -} ->
  Image       {- ^ activity bar, status bar -}
statusLineImage st = content <|> charFill defAttr '─' fillSize 1
  where
    fillSize = max 0 (view clientWidth st - imageWidth content)
    content = horizCat
      [ myNickImage st
      , focusImage st
      , activitySummary st
      , detailImage st
      , nometaImage st
      , scrollImage st
      , filterImage st
      , latencyImage st
      ]

minorStatusLineImage :: Focus -> ClientState -> Image
minorStatusLineImage focus st =
  content <|> charFill defAttr '─' fillSize 1
  where
    content = infoBubble (focusImageMajor focus st)
    fillSize = max 0 (view clientWidth st - imageWidth content)


scrollImage :: ClientState -> Image
scrollImage st
  | 0 == view clientScroll st = emptyImage
  | otherwise = infoBubble (string attr "scroll")
  where
    pal  = clientPalette st
    attr = view palLabel pal

filterImage :: ClientState -> Image
filterImage st =
  case clientActiveRegex st of
    Nothing -> emptyImage
    Just {} -> infoBubble (string attr "filtered")
  where
    pal  = clientPalette st
    attr = view palLabel pal

latencyImage :: ClientState -> Image
latencyImage st
  | Just network <- views clientFocus focusNetwork st
  , Just cs      <- preview (clientConnection network) st =
  case view csPingStatus cs of
    PingNever -> emptyImage
    PingSent {} -> infoBubble (string (view palLatency pal) "sent")
    PingLatency delta ->
      infoBubble (string (view palLatency pal) (showFFloat (Just 2) delta "s"))
    PingConnecting n _ ->
      infoBubble (string (view palLabel pal) "connecting" <|> retryImage)
      where
        retryImage
          | n > 0 = string defAttr ": " <|>
                    string (view palLabel pal) ("retry " ++ show n)
          | otherwise = emptyImage
  | otherwise = emptyImage
  where
    pal = clientPalette st

infoBubble :: Image -> Image
infoBubble img = string defAttr "─(" <|> img <|> string defAttr ")"

detailImage :: ClientState -> Image
detailImage st
  | view clientDetailView st = infoBubble (string attr "detail")
  | otherwise = emptyImage
  where
    pal  = clientPalette st
    attr = view palLabel pal

nometaImage :: ClientState -> Image
nometaImage st
  | view clientShowMetadata st = emptyImage
  | otherwise = infoBubble (string attr "nometa")
  where
    pal  = clientPalette st
    attr = view palLabel pal

-- | Image for little box with active window names:
--
-- @-[15p]@
activitySummary :: ClientState -> Image
activitySummary st
  | null indicators = emptyImage
  | otherwise       = string defAttr "─[" <|>
                      horizCat indicators <|>
                      string defAttr "]"
  where
    winNames = clientWindowNames st ++ repeat '?'

    indicators = foldr aux [] (zip winNames windows)
    windows    = views clientWindows Map.elems st

    aux (i,w) rest
      | view winUnread w == 0 = rest
      | otherwise = char attr i : rest
      where
        pal = clientPalette st
        attr | view winMention w = view palMention pal
             | otherwise         = view palActivity pal

-- | Multi-line activity information enabled by F3
activityBarImage :: ClientState -> Image
activityBarImage st
  | view clientActivityBar st = activityBar'
  | otherwise                 = emptyImage
  where
    activityBar' = makeLines (view clientWidth st)
                 $ catMaybes
                 $ zipWith baraux winNames
                 $ Map.toList
                 $ view clientWindows st

    winNames = clientWindowNames st ++ repeat '?'

    baraux i (focus,w)
      | n == 0 = Nothing -- todo: make configurable
      | otherwise = Just
                  $ string defAttr "─[" <|>
                    char (view palWindowName pal) i <|>
                    char defAttr              ':' <|>
                    text' (view palLabel pal) focusText <|>
                    char defAttr              ':' <|>
                    string attr               (show n) <|>
                    string defAttr "]"
      where
        n   = view winUnread w
        pal = clientPalette st
        attr | view winMention w = view palMention pal
             | otherwise         = view palActivity pal
        focusText =
          case focus of
            Unfocused           -> Text.pack "*"
            NetworkFocus net    -> net
            ChannelFocus _ chan -> idText chan



makeLines ::
  Int     {- ^ window width       -} ->
  [Image] {- ^ components to pack -} ->
  Image
makeLines _ [] = emptyImage
makeLines w (x:xs) = go x xs
  where

    go acc (y:ys)
      | let acc' = acc <|> y
      , imageWidth acc' <= w
      = go acc' ys

    go acc ys = makeLines w ys
            <-> acc <|> charFill defAttr '─' (max 0 (w - imageWidth acc)) 1


myNickImage :: ClientState -> Image
myNickImage st =
  case view clientFocus st of
    NetworkFocus network      -> nickPart network Nothing
    ChannelFocus network chan -> nickPart network (Just chan)
    Unfocused                 -> emptyImage
  where
    pal = clientPalette st
    nickPart network mbChan =
      case preview (clientConnection network) st of
        Nothing -> emptyImage
        Just cs -> string (view palSigil pal) myChanModes
               <|> text' defAttr (idText nick)
               <|> parens defAttr (string defAttr ('+' : view csModes cs))
          where
            nick      = view csNick cs
            myChanModes =
              case mbChan of
                Nothing   -> []
                Just chan -> view (csChannels . ix chan . chanUsers . ix nick) cs


focusImage :: ClientState -> Image
focusImage st =
    infoBubble (focusImageMajor focus st) <|>
    foldMap infoBubble (viewSubfocusLabel pal subfocus)
  where

    !pal        = clientPalette st
    focus       = view clientFocus st
    subfocus    = view clientSubfocus st

focusImageMajor :: Focus -> ClientState -> Image
focusImageMajor focus st =
  horizCat
    [ char (view palWindowName pal) windowName
    , char defAttr ':'
    , viewFocusLabel st focus
    ]
  where
    !pal        = clientPalette st
    windowNames = clientWindowNames st

    windowName = fromMaybe '?'
               $ do i <- Map.lookupIndex focus (view clientWindows st)
                    preview (ix i) windowNames


parens :: Attr -> Image -> Image
parens attr i = char attr '(' <|> i <|> char attr ')'

viewFocusLabel :: ClientState -> Focus -> Image
viewFocusLabel st focus =
  let !pal = clientPalette st in
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

viewSubfocusLabel :: Palette -> Subfocus -> Maybe Image
viewSubfocusLabel pal subfocus =
  case subfocus of
    FocusMessages -> Nothing
    FocusWindows  -> Just $ string (view palLabel pal) "windows"
    FocusInfo     -> Just $ string (view palLabel pal) "info"
    FocusUsers    -> Just $ string (view palLabel pal) "users"
    FocusMentions -> Just $ string (view palLabel pal) "mentions"
    FocusPalette  -> Just $ string (view palLabel pal) "palette"
    FocusHelp mb  -> Just $ string (view palLabel pal) "help" <|>
                            foldMap (\cmd -> char defAttr ':' <|>
                                        text' (view palLabel pal) cmd) mb
    FocusMasks m  -> Just $ horizCat
      [ string (view palLabel pal) "masks"
      , char defAttr ':'
      , char (view palLabel pal) m
      ]
