module Client.Image (clientPicture) where

import           Client.ChannelState
import           Client.ConnectionState
import           Client.MircFormatting
import           Client.State
import           Client.Window
import           Client.MessageRenderer
import           Control.Lens
import           Data.List
import           Data.Ord
import           Graphics.Vty (Picture(..), Cursor(..), picForImage)
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier, idText)
import           Client.Message
import qualified Client.EditBox as Edit
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time

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
            , horizDividerImage st
            , textboxImage st
            ]

messagePaneImages :: ClientState -> [Image]
messagePaneImages st =
  case (view clientFocus st, view clientSubfocus st) of
    (ChannelFocus network channel, FocusUsers) ->
      userListImages network channel st
    (ChannelFocus network channel, FocusMasks mode) ->
      maskListImages mode network channel st
    -- subfocuses only make sense for channels
    (focus, _) -> windowLineProcessor
                $ view (clientWindows . ix focus . winMessages) st
      where
        windowLineProcessor
          | view clientDetailView st = map (view wlFullImage)
          | otherwise                = windowLinesToImages

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

windowLinesToImages :: [WindowLine] -> [Image]
windowLinesToImages wwls =
  case wwls of
    [] -> []
    wl:wls
      | Just (img,ident) <- metadataWindowLine wl -> windowLinesToImagesMd img ident wls
      | otherwise -> view wlImage wl : windowLinesToImages wls

windowLinesToImagesMd :: Image -> Identifier -> [WindowLine] -> [Image]
windowLinesToImagesMd acc who wwls =
  case wwls of
    wl:wls
      | Just (img,ident) <- metadataWindowLine wl ->
          if who == ident
            then windowLinesToImagesMd (acc <|> img) who wls
            else windowLinesToImagesMd (finish <|> char defAttr ' ' <|> img) ident wls
    _ -> finish : windowLinesToImages wwls
  where
    finish = acc <|> quietIdentifier who


metadataWindowLine :: WindowLine -> Maybe (Image, Identifier)
metadataWindowLine wl =
  case view wlBody wl of
    IrcBody irc -> metadataImg irc
    _           -> Nothing

lineWrap :: Int -> Image -> Image
lineWrap w img
  | imageWidth img > w = cropRight w img <-> lineWrap w (cropLeft (imageWidth img - w) img)
  | otherwise = img




horizDividerImage :: ClientState -> Image
horizDividerImage st
  = content <|> charFill defAttr '─' fillSize 1
  where
    fillSize = max 0 (view clientWidth st - imageWidth content)
    content = horizCat
      [ myNickImage st
      , parens defAttr (focusImage st)
      , activityImage st
      ]

parens :: Attr -> Image -> Image
parens attr i = char attr '(' <|> i <|> char attr ')'

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
      case view (clientConnections . at network) st of
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
focusImage st =
  case view clientFocus st of
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
  case preview (clientConnections . ix network . csChannels . ix channel . chanModes) st of
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

userListImages :: NetworkName -> Identifier -> ClientState -> [Image]
userListImages network channel st =
    [horizCat (intersperse gap (map renderUser usersList))]
  where
    renderUser (ident, sigils) =
      string (withForeColor defAttr cyan) sigils <|>
      text' defAttr (idText ident)

    gap = char defAttr ' '

    usersList = sortBy (comparing fst) (HashMap.toList usersHashMap)
    usersHashMap =
      view ( clientConnections . ix network
           . csChannels        . ix channel
           . chanUsers ) st

maskListImages :: Char -> NetworkName -> Identifier -> ClientState -> [Image]
maskListImages mode network channel st
  | null entryList = [string (withForeColor defAttr red) "No masks"]
  | otherwise = [ text' defAttr user <|> char defAttr ' ' <|>
                  text' defAttr who  <|> char defAttr ' ' <|>
                  string defAttr (formatTime defaultTimeLocale "%F %T" when)
                | (user, (who,when)) <- entryList ]
  where
    entryList = sortBy (comparing (snd . snd)) (HashMap.toList entries)
    entries = view ( clientConnections . ix network
                   . csChannels        . ix channel
                   . chanList mode
                   ) st
