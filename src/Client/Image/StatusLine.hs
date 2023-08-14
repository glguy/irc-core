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
  , clientTitle
  ) where

import Client.Image.Message (cleanChar, cleanText, IdentifierColorMode (NormalIdentifier), coloredIdentifier)
import Client.Image.PackedImage
import Client.Image.Palette
import Client.State
import Client.State.Channel (chanModes, chanUsers)
import Client.State.Focus (focusNetwork, Focus(..), Subfocus(..), WindowsFilter(..))
import Client.State.Network
import Client.State.Window
import Control.Lens (view, orOf, preview, views, _Just, At(at), Ixed(ix))
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict qualified as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Graphics.Vty.Attributes (Attr, defAttr, bold, withForeColor, withStyle, red)
import Graphics.Vty.Image qualified as Vty
import Irc.Identifier (idText)
import Numeric (showFFloat)

clientTitle :: ClientState -> String
clientTitle st
  = map cleanChar
  $ LText.unpack
  $ "glirc - " <> imageText (viewFocusLabel st (view clientFocus st))

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
      , subfocusImage (view clientSubfocus st) st
      , detailImage st
      , nometaImage (view clientFocus st) st
      , scrollImage st
      , filterImage st
      , lockImage st
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
  Focus       {- ^ window name          -} ->
  Subfocus    {- ^ subfocus             -} ->
  Int         {- ^ draw width           -} ->
  Bool        {- ^ show hidemeta status -} ->
  ClientState {- ^ client state -} ->
  Image'
minorStatusLineImage focus subfocus w showHideMeta st =
  content <> mconcat (replicate fillSize bar)
  where
    content = focusImage focus st <>
              subfocusImage subfocus st <>
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
filterImage st
  | clientIsFiltered st = infoBubble (string attr "filtered")
  | otherwise           = mempty
  where
    pal  = clientPalette st
    attr = view palError pal

-- | Indicate when the client editor is locked
lockImage :: ClientState -> Image'
lockImage st
  | view clientEditLock st = infoBubble (string attr "locked")
  | otherwise              = mempty
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

       PingConnecting n _ _ ->
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
    indicators = foldr aux [] windows
    windows    = views clientWindows Map.elems st

    aux w rest =
      let name = case view winName w of
                   Nothing -> '?'
                   Just i -> i in
      if view winSilent w then rest else
      case view winMention w of
        WLImportant -> Vty.char (view palMention  pal) name : rest
        WLNormal    -> Vty.char (view palActivity pal) name : rest
        WLBoring    -> rest
      where
        pal = clientPalette st

-- | Multi-line activity information enabled by F3
activityBarImages :: ClientState -> [Vty.Image]
activityBarImages st
  = mapMaybe baraux
  $ Map.toAscList
  $ view clientWindows st

  where
    baraux (focus,w)
      | view winSilent w = Nothing
      | n == 0 = Nothing -- todo: make configurable
      | otherwise = Just
                  $ unpackImage bar Vty.<|>
                    Vty.char defAttr '[' Vty.<|>
                    jumpLabel Vty.<|>
                    focusLabel Vty.<|>
                    Vty.char defAttr ':' Vty.<|>
                    Vty.string attr (show n) Vty.<|>
                    Vty.char defAttr ']'
      where
        jumpLabel =
          case view winName w of
            Nothing   -> mempty
            Just name -> Vty.char (view palWindowName pal) name Vty.<|>
                         Vty.char defAttr ':'
        n   = view winUnread w
        pal = clientPalette st
        attr = case view winMention w of
                 WLImportant -> view palMention pal
                 _           -> view palActivity pal
        focusLabel =
          unpackImage $ case focus of
            Unfocused           -> text' (view palLabel pal) (Text.pack "*")
            NetworkFocus net    -> text' (view palLabel pal) (cleanText net)
            ChannelFocus _ chan -> coloredIdentifier pal NormalIdentifier HashMap.empty chan


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
    NetworkFocus network      -> nickPart network
    ChannelFocus network _    -> nickPart network
    Unfocused                 -> Vty.emptyImage
  where
    pal = clientPalette st
    nickPart network =
      case preview (clientConnection network) st of
        Nothing -> Vty.emptyImage
        Just cs -> Vty.text' attr (cleanText (idText nick))
           Vty.<|> parens defAttr
                     (unpackImage $
                      modesImage (view palUModes pal) (view csModes cs) <>
                      snomaskImage)
          where
            attr
              | view csAway cs = view palAway pal
              | otherwise      = defAttr

            nick = view csNick cs

            snomaskImage
              | null (view csSnomask cs) = ""
              | otherwise                = " " <> modesImage (view palSnomask pal) (view csSnomask cs)

modesImage :: HashMap Char Attr -> String -> Image'
modesImage pal modes = "+" <> foldMap modeImage modes
  where
    modeImage m =
      char (fromMaybe defAttr (view (at m) pal)) m

subfocusImage :: Subfocus -> ClientState -> Image'
subfocusImage subfocus st = foldMap infoBubble (viewSubfocusLabel pal subfocus)
  where
    pal         = clientPalette st

focusImage :: Focus -> ClientState -> Image'
focusImage focus st =
  infoBubble $
  case preview (clientWindows . ix focus . winName . _Just) st of
    Nothing -> label
    Just n  -> char (view palWindowName pal) n <> ":" <> label
  where
    !pal        = clientPalette st
    label       = viewFocusLabel st focus

parens :: Attr -> Vty.Image -> Vty.Image
parens attr i = Vty.char attr '(' Vty.<|> i Vty.<|> Vty.char attr ')'

viewFocusLabel :: ClientState -> Focus -> Image'
viewFocusLabel st focus =
  let !pal = clientPalette st in
  case focus of
    Unfocused ->
      char (view palError pal) '*'
    NetworkFocus network ->
      text' (view palLabel pal) (cleanText network)
    ChannelFocus network channel ->
      text' (view palLabel pal) (cleanText network) <>
      char defAttr ':' <>
      string (view palSigil pal) (cleanChar <$> sigils) <>
      coloredIdentifier pal NormalIdentifier HashMap.empty channel <>
      channelModes

      where
        (sigils, channelModes) =
          case preview (clientConnection network) st of
            Nothing -> ("", mempty)
            Just cs ->
               ( let nick = view csNick cs in
                 view (csChannels . ix channel . chanUsers . ix nick) cs

               , case preview (csChannels . ix channel . chanModes) cs of
                    Just modeMap | not (null modeMap) ->
                        " " <> modesImage (view palCModes pal) (Map.keys modeMap)
                    _ -> mempty
               )

viewSubfocusLabel :: Palette -> Subfocus -> Maybe Image'
viewSubfocusLabel pal subfocus =
  case subfocus of
    FocusMessages     -> Nothing
    FocusWindows filt -> Just $ string (view palLabel pal) "windows" <>
                                opt (windowFilterName filt)
    FocusInfo         -> Just $ string (view palLabel pal) "info"
    FocusUsers        -> Just $ string (view palLabel pal) "users"
    FocusMentions     -> Just $ string (view palLabel pal) "mentions"
    FocusPalette      -> Just $ string (view palLabel pal) "palette"
    FocusDigraphs     -> Just $ string (view palLabel pal) "digraphs"
    FocusKeyMap       -> Just $ string (view palLabel pal) "keymap"
    FocusHelp mb      -> Just $ string (view palLabel pal) "help" <> opt mb
    FocusIgnoreList   -> Just $ string (view palLabel pal) "ignores"
    FocusRtsStats     -> Just $ string (view palLabel pal) "rtsstats"
    FocusCert{}       -> Just $ string (view palLabel pal) "cert"
    FocusChanList _ _ -> Just $ string (view palLabel pal) "channels"
    FocusMasks m      -> Just $ mconcat
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
