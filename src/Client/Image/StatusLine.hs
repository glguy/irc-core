{-# Language OverloadedStrings, BangPatterns #-}
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
  ) where

import           Client.Image.Message (cleanText)
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
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier, idText)
import           Numeric

-- | Renders the status line between messages and the textbox.
statusLineImage ::
  ClientState {- ^ client state -} ->
  Image       {- ^ status bar   -}
statusLineImage st = makeLines (view clientWidth st)
                   $ common : activity ++ errorImgs
  where
    w = view clientWidth st

    common = horizCat
      [ myNickImage st
      , focusImage st
      , detailImage st
      , nometaImage st
      , scrollImage st
      , filterImage st
      , latencyImage st
      ]

    activity
      | view clientActivityBar st = activityBarImages st
      | otherwise                 = [activitySummary st]

    errorImgs =
      transientErrorImage <$> maybeToList (view clientErrorMsg st)


lineExtend :: Int -> Image -> Image
lineExtend w img = img <|> charFill defAttr '─' fillSize 1
  where fillSize = max 0 (w - imageWidth img)


-- Generates an error message notification image.
transientErrorImage ::
  Text  {- ^ @error-message@           -} ->
  Image {- ^ @─[error: error-message]@ -}
transientErrorImage txt =
  text' defAttr "─[" <|>
  text' (withForeColor defAttr red) "error: " <|>
  text' defAttr (cleanText txt) <|>
  text' defAttr "]"


-- | The minor status line is used when rendering the @/splits@ and
-- @/mentions@ views to show the associated window name.
minorStatusLineImage :: Focus -> ClientState -> Image
minorStatusLineImage focus st =
  content <|> charFill defAttr '─' fillSize 1
  where
    content = infoBubble (focusImageMajor focus st)
    fillSize = max 0 (view clientWidth st - imageWidth content)


-- | Indicate when the client is scrolling and old messages are being shown.
scrollImage :: ClientState -> Image
scrollImage st
  | 0 == view clientScroll st = emptyImage
  | otherwise = infoBubble (string attr "scroll")
  where
    pal  = clientPalette st
    attr = view palLabel pal


-- | Indicate when the client is potentially showing a subset of the
-- available chat messages.
filterImage :: ClientState -> Image
filterImage st =
  case clientActiveRegex st of
    Nothing -> emptyImage
    Just {} -> infoBubble (string attr "filtered")
  where
    pal  = clientPalette st
    attr = view palLabel pal


-- | Indicate the current connection health. This will either indicate
-- that the connection is being established or that a ping has been
-- sent or long the previous ping round-trip was.
latencyImage :: ClientState -> Image
latencyImage st =
  case views clientFocus focusNetwork st of
    Nothing      -> emptyImage
    Just network ->
      case preview (clientConnection network) st of
        Nothing -> infoBubble (string (view palError pal) "offline")
        Just cs ->
          case view csPingStatus cs of
            PingNever          -> emptyImage
            PingSent {}        -> latency "ping sent"
            PingLatency delta  -> latency (showFFloat (Just 2) delta "s")
            PingConnecting n _ ->
              infoBubble (string (view palLatency pal) "connecting" <|>
                          retryImage n)
  where
    pal     = clientPalette st
    latency = infoBubble . string (view palLatency pal)

    retryImage n
      | n > 0     = string defAttr ": " <|>
                    string (view palLabel pal) ("retry " ++ show n)
      | otherwise = emptyImage


-- | Wrap some text in parentheses to make it suitable for inclusion in the
-- status line.
infoBubble :: Image -> Image
infoBubble img = string defAttr "─(" <|> img <|> string defAttr ")"


-- | Indicate that the client is in the /detailed/ view.
detailImage :: ClientState -> Image
detailImage st
  | view clientDetailView st = infoBubble (string attr "detail")
  | otherwise = emptyImage
  where
    pal  = clientPalette st
    attr = view palLabel pal


-- | Indicate that the client isn't showing the metadata lines in /normal/
-- view.
nometaImage :: ClientState -> Image
nometaImage st
  | metaHidden = infoBubble (string attr "nometa")
  | otherwise  = emptyImage
  where
    pal        = clientPalette st
    attr       = view palLabel pal
    focus      = view clientFocus st
    metaHidden = orOf (clientWindows . ix focus . winHideMeta) st

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

    aux (i,w) rest =
      case view winMention w of
        WLImportant -> char (view palMention  pal) i : rest
        WLNormal    -> char (view palActivity pal) i : rest
        WLBoring    -> rest
      where
        pal = clientPalette st

-- | Multi-line activity information enabled by F3
activityBarImages :: ClientState -> [Image]
activityBarImages st
  = catMaybes
  $ zipWith baraux winNames
  $ Map.toList
  $ view clientWindows st

  where

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
        attr = case view winMention w of
                 WLImportant -> view palMention pal
                 _           -> view palActivity pal
        focusText =
          case focus of
            Unfocused           -> Text.pack "*"
            NetworkFocus net    -> net
            ChannelFocus _ chan -> idText chan


-- | Pack a list of images into a single image spanning possibly many lines.
-- The images will stack upward with the first element of the list being in
-- the bottom left corner of the image. Each line will have at least one
-- of the component images in it, which might truncate that image in extreme
-- cases.
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
    FocusWindows filt -> Just $ string (view palLabel pal) "windows" <|>
                                opt (windowFilterName filt)
    FocusInfo     -> Just $ string (view palLabel pal) "info"
    FocusUsers    -> Just $ string (view palLabel pal) "users"
    FocusMentions -> Just $ string (view palLabel pal) "mentions"
    FocusPalette  -> Just $ string (view palLabel pal) "palette"
    FocusDigraphs -> Just $ string (view palLabel pal) "digraphs"
    FocusKeyMap   -> Just $ string (view palLabel pal) "keymap"
    FocusHelp mb  -> Just $ string (view palLabel pal) "help" <|>
                            opt mb
    FocusMasks m  -> Just $ horizCat
      [ string (view palLabel pal) "masks"
      , char defAttr ':'
      , char (view palLabel pal) m
      ]
  where
    opt = foldMap (\cmd -> char defAttr ':' <|>
                           text' (view palLabel pal) cmd)

windowFilterName :: WindowsFilter -> Maybe Text
windowFilterName x =
  case x of
    AllWindows     -> Nothing
    NetworkWindows -> Just "networks"
    ChannelWindows -> Just "channels"
    UserWindows    -> Just "users"
