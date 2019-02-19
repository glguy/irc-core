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
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Channel
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Control.Lens
import           Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.Vty.Attributes
import qualified Graphics.Vty.Image as Vty
import           Irc.Identifier (Identifier, idText)
import           Numeric

bar :: Image'
bar = char (withStyle defAttr bold) '─'


-- | Renders the status line between messages and the textbox.
statusLineImage ::
  Int         {- ^ draw width   -} ->
  ClientState {- ^ client state -} ->
  Vty.Image   {- ^ status bar   -}
statusLineImage w st =
  makeLines w (common : activity ++ errorImgs)
  where
    common = Vty.horizCat $
      myNickImage st :
      map unpackImage
      [ focusImage (view clientFocus st) st
      , subfocusImage st
      , detailImage st
      , nometaImage (view clientFocus st) st
      , scrollImage st
      , filterImage st
      , latency
      ]

    latency
      | view clientShowPing st = latencyImage st
      | otherwise              = mempty

    activity
      | view clientActivityBar st = activityBarImages st
      | otherwise                 = [activitySummary st]

    errorImgs =
      transientErrorImage <$> maybeToList (view clientErrorMsg st)


-- Generates an error message notification image.
transientErrorImage ::
  Text  {- ^ @error-message@           -} ->
  Vty.Image {- ^ @─[error: error-message]@ -}
transientErrorImage txt =
  Vty.text' defAttr "─[" Vty.<|>
  Vty.text' (withForeColor defAttr red) "error: " Vty.<|>
  Vty.text' defAttr (cleanText txt) Vty.<|>
  Vty.text' defAttr "]"


-- | The minor status line is used when rendering the @/splits@ and
-- @/mentions@ views to show the associated window name.
minorStatusLineImage ::
  Focus {- ^ window name          -} ->
  Int   {- ^ draw width           -} ->
  Bool  {- ^ show hidemeta status -} ->
  ClientState {- ^ client state -} ->
  Image'
minorStatusLineImage focus w showHideMeta st =
  content <> mconcat (replicate fillSize bar)
  where
    content = focusImage focus st <>
              if showHideMeta then nometaImage focus st else mempty

    fillSize = max 0 (w - imageWidth content)


-- | Indicate when the client is scrolling and old messages are being shown.
scrollImage :: ClientState -> Image'
scrollImage st
  | 0 == view clientScroll st = mempty
  | otherwise = infoBubble (string attr "scroll")
  where
    pal  = clientPalette st
    attr = view palError pal


-- | Indicate when the client is potentially showing a subset of the
-- available chat messages.
filterImage :: ClientState -> Image'
filterImage st =
  case clientMatcher st of
    Nothing -> mempty
    Just {} -> infoBubble (string attr "filtered")
  where
    pal  = clientPalette st
    attr = view palError pal


-- | Indicate the current connection health. This will either indicate
-- that the connection is being established or that a ping has been
-- sent or long the previous ping round-trip was.
latencyImage :: ClientState -> Image'
latencyImage st = either id id $

  do network <- -- no network -> no image
       case views clientFocus focusNetwork st of
         Nothing  -> Left mempty
         Just net -> Right net

     cs <- -- detect when offline
       case preview (clientConnection network) st of
         Nothing -> Left (infoBubble (string (view palError pal) "offline"))
         Just cs -> Right cs

     -- render latency if one is stored
     for_ (view csLatency cs) $ \latency ->
       Left (latencyBubble (showFFloat (Just 2) (realToFrac latency :: Double) "s"))

     Right $ case view csPingStatus cs of

       PingSent {} -> latencyBubble "wait"

       PingConnecting n _ ->
         infoBubble (string (view palLatency pal) "connecting" <> retryImage n)

       PingNone -> mempty -- just connected no ping sent yet

  where
    pal           = clientPalette st
    latencyBubble = infoBubble . string (view palLatency pal)

    retryImage n
      | n > 0     = ": " <> string (view palLabel pal) ("retry " ++ show n)
      | otherwise = mempty


-- | Wrap some text in parentheses to make it suitable for inclusion in the
-- status line.
infoBubble :: Image' -> Image'
infoBubble img = bar <> "(" <> img <> ")"


-- | Indicate that the client is in the /detailed/ view.
detailImage :: ClientState -> Image'
detailImage st
  | view clientDetailView st = infoBubble (string attr "detail")
  | otherwise = mempty
  where
    pal  = clientPalette st
    attr = view palLabel pal


-- | Indicate that the client isn't showing the metadata lines in /normal/
-- view.
nometaImage :: Focus -> ClientState -> Image'
nometaImage focus st
  | metaHidden = infoBubble (string attr "nometa")
  | otherwise  = mempty
  where
    pal        = clientPalette st
    attr       = view palLabel pal
    metaHidden = orOf (clientWindows . ix focus . winHideMeta) st

-- | Image for little box with active window names:
--
-- @-[15p]@
activitySummary :: ClientState -> Vty.Image
activitySummary st
  | null indicators = Vty.emptyImage
  | otherwise       = unpackImage bar Vty.<|>
                      Vty.string defAttr "[" Vty.<|>
                      Vty.horizCat indicators Vty.<|>
                      Vty.string defAttr "]"
  where
    winNames = clientWindowNames st ++ repeat '?'

    indicators = foldr aux [] (zip winNames windows)
    windows    = views clientWindows Map.elems st

    aux (i,w) rest =
      case view winMention w of
        WLImportant -> Vty.char (view palMention  pal) i : rest
        WLNormal    -> Vty.char (view palActivity pal) i : rest
        WLBoring    -> rest
      where
        pal = clientPalette st

-- | Multi-line activity information enabled by F3
activityBarImages :: ClientState -> [Vty.Image]
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
                  $ unpackImage bar Vty.<|>
                    Vty.char defAttr '[' Vty.<|>
                    Vty.char (view palWindowName pal) i Vty.<|>
                    Vty.char defAttr ':' Vty.<|>
                    Vty.text' (view palLabel pal) focusText Vty.<|>
                    Vty.char defAttr ':' Vty.<|>
                    Vty.string attr (show n) Vty.<|>
                    Vty.char defAttr ']'
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
  [Vty.Image] {- ^ components to pack -} ->
  Vty.Image
makeLines _ [] = Vty.emptyImage
makeLines w (x:xs) = go x xs
  where
    go acc (y:ys)
      | let acc' = acc Vty.<|> y
      , Vty.imageWidth acc' <= w
      = go acc' ys

    go acc ys = makeLines w ys
        Vty.<-> Vty.cropRight w acc
        Vty.<|> unpackImage (mconcat (replicate fillsize bar))
      where
        fillsize = max 0 (w - Vty.imageWidth acc)


myNickImage :: ClientState -> Vty.Image
myNickImage st =
  case view clientFocus st of
    NetworkFocus network      -> nickPart network Nothing
    ChannelFocus network chan -> nickPart network (Just chan)
    Unfocused                 -> Vty.emptyImage
  where
    pal = clientPalette st
    nickPart network mbChan =
      case preview (clientConnection network) st of
        Nothing -> Vty.emptyImage
        Just cs -> Vty.string (view palSigil pal) myChanModes
           Vty.<|> Vty.text' defAttr (idText nick)
           Vty.<|> parens defAttr (Vty.string defAttr ('+' : view csModes cs))
          where
            nick      = view csNick cs
            myChanModes =
              case mbChan of
                Nothing   -> []
                Just chan -> view (csChannels . ix chan . chanUsers . ix nick) cs


subfocusImage :: ClientState -> Image'
subfocusImage st = foldMap infoBubble (viewSubfocusLabel pal subfocus)
  where
    pal         = clientPalette st
    subfocus    = view clientSubfocus st

focusImage :: Focus -> ClientState -> Image'
focusImage focus st = infoBubble $ mconcat
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


parens :: Attr -> Vty.Image -> Vty.Image
parens attr i = Vty.char attr '(' Vty.<|> i Vty.<|> Vty.char attr ')'

viewFocusLabel :: ClientState -> Focus -> Image'
viewFocusLabel st focus =
  let !pal = clientPalette st in
  case focus of
    Unfocused ->
      char (view palError pal) '*'
    NetworkFocus network ->
      text' (view palLabel pal) network
    ChannelFocus network channel ->
      text' (view palLabel pal) network <>
      char defAttr ':' <>
      text' (view palLabel pal) (idText channel) <>
      channelModesImage network channel st

channelModesImage :: Text -> Identifier -> ClientState -> Image'
channelModesImage network channel st =
  case preview (clientConnection network . csChannels . ix channel . chanModes) st of
    Just modeMap | not (null modeMap) -> string defAttr (" +" ++ Map.keys modeMap)
    _                                 -> mempty

viewSubfocusLabel :: Palette -> Subfocus -> Maybe Image'
viewSubfocusLabel pal subfocus =
  case subfocus of
    FocusMessages -> Nothing
    FocusWindows filt -> Just $ string (view palLabel pal) "windows" <>
                                opt (windowFilterName filt)
    FocusInfo     -> Just $ string (view palLabel pal) "info"
    FocusUsers    -> Just $ string (view palLabel pal) "users"
    FocusMentions -> Just $ string (view palLabel pal) "mentions"
    FocusPalette  -> Just $ string (view palLabel pal) "palette"
    FocusDigraphs -> Just $ string (view palLabel pal) "digraphs"
    FocusKeyMap   -> Just $ string (view palLabel pal) "keymap"
    FocusHelp mb  -> Just $ string (view palLabel pal) "help" <>
                            opt mb
    FocusIgnoreList -> Just $ string (view palLabel pal) "ignores"
    FocusRtsStats -> Just $ string (view palLabel pal) "rtsstats"
    FocusMasks m  -> Just $ mconcat
      [ string (view palLabel pal) "masks"
      , char defAttr ':'
      , char (view palLabel pal) m
      ]
  where
    opt = foldMap (\cmd -> char defAttr ':' <>
                           text' (view palLabel pal) cmd)

windowFilterName :: WindowsFilter -> Maybe Text
windowFilterName x =
  case x of
    AllWindows     -> Nothing
    NetworkWindows -> Just "networks"
    ChannelWindows -> Just "channels"
    UserWindows    -> Just "users"
