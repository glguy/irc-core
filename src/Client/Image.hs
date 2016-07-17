module Client.Image (clientPicture) where

import           Client.ChannelState
import           Client.ConnectionState
import           Client.MircFormatting
import           Client.State
import           Client.Window
import           Control.Lens
import           Data.List
import           Graphics.Vty (Picture(..), Cursor(..), picForImage)
import           Graphics.Vty.Image
import           Irc.Identifier (Identifier, idText)
import qualified Client.EditBox as Edit
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text

clientPicture :: ClientState -> Picture
clientPicture st =
  (picForImage (clientImage st)) { picCursor = cursor }
    where
      cursor = Cursor (min (view clientWidth st - 1)
                           (view (clientTextBox . Edit.pos) st+1))
                      (view clientHeight st - 1)

clientImage :: ClientState -> Image
clientImage st = vertCat
   [ messagePane st
   , horizDividerImage st
   , textboxImage st
   ]

messagePane :: ClientState -> Image
messagePane st = assemble emptyImage (view wlImage <$> messages)
  where
    -- Failure returns empty list due to monoid instance on [a]
    messages = view (clientWindows . ix (view clientFocus st) . winMessages) st

    assemble acc _ | imageHeight acc >= h = cropTop h acc
    assemble acc [] = pad 0 (h - imageHeight acc) 0 0 acc
    assemble acc (x:xs) = assemble (lineWrap w x <-> acc) xs

    h = view clientHeight st - 2
    w = view clientWidth st

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
                      horizCat (intersperse (char defAttr ',') indicators) <|>
                      string defAttr "]"
  where
    windows = views clientWindows Map.elems st
    indicators = aux (zip [1::Int ..] windows)
    aux [] = []
    aux ((i,w):ws)
      | view winUnread w == 0 = aux ws
      | otherwise = string (withForeColor defAttr color) (show i) : aux ws
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
