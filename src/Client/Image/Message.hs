{-# Language OverloadedStrings, BangPatterns #-}
{-|
Module      : Client.Image.Message
Description : Renderer for message lines
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides image renderers for messages.

-}
module Client.Image.Message
  ( MessageRendererParams(..)
  , RenderMode(..)
  , IdentifierColorMode(..)
  , defaultRenderParams
  , msgImage
  , metadataImg
  , ignoreImage
  , quietIdentifier
  , coloredUserInfo
  , coloredIdentifier
  , cleanText
  , cleanChar
  , rightPad
  ) where

import           Client.Image.MircFormatting
import           Client.Image.Palette
import           Client.Message
import           Control.Lens
import           Data.Char
import           Data.Hashable (hash)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time
import qualified Data.Vector as Vector
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image
import           Irc.Codes
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo

-- | Parameters used when rendering messages
data MessageRendererParams = MessageRendererParams
  { rendStatusMsg  :: [Char] -- ^ restricted message sigils
  , rendUserSigils :: [Char] -- ^ sender sigils
  , rendNicks      :: HashSet Identifier -- ^ nicknames to highlight
  , rendMyNicks    :: HashSet Identifier -- ^ nicknames to highlight in red
  , rendPalette    :: Palette -- ^ nick color palette
  , rendNickPadding :: Maybe Integer -- ^ nick padding
  }

-- | Default 'MessageRendererParams' with no sigils or nicknames specified
defaultRenderParams :: MessageRendererParams
defaultRenderParams = MessageRendererParams
  { rendStatusMsg   = ""
  , rendUserSigils  = ""
  , rendNicks       = HashSet.empty
  , rendMyNicks     = HashSet.empty
  , rendPalette     = defaultPalette
  , rendNickPadding = Nothing
  }

-- | Construct a message given the time the message was received and its
-- render parameters.
msgImage ::
  RenderMode ->
  ZonedTime {- ^ time of message -} ->
  MessageRendererParams -> MessageBody -> Image
msgImage rm when params body = horizCat
  [ renderTime rm (rendPalette params) when
  , statusMsgImage (rendStatusMsg params)
  , bodyImage rm params body
  ]

cleanChar :: Char -> Char
cleanChar x
  | x < '\x20'  = chr (0x2400 + ord x)
  | x == '\DEL' = '␡'
  | isControl x = '�'
  | otherwise   = x

cleanText :: Text -> Text
cleanText = Text.map cleanChar

errorImage ::
  MessageRendererParams ->
  Text {- ^ error message -} ->
  Image
errorImage params txt = horizCat
  [ text' (view palError (rendPalette params)) "error "
  , text' defAttr (cleanText txt)
  ]

normalImage ::
  MessageRendererParams ->
  Text {- ^ message -} ->
  Image
normalImage params txt = horizCat
  [ text' (view palLabel (rendPalette params)) "client "
  , text' defAttr (cleanText txt)
  ]

-- | Render the given time according to the current mode and palette.
renderTime :: RenderMode -> Palette -> ZonedTime -> Image
renderTime DetailedRender = datetimeImage
renderTime NormalRender   = timeImage

-- | Render the sigils for a restricted message.
statusMsgImage :: [Char] {- ^ sigils -} -> Image
statusMsgImage modes
  | null modes = emptyImage
  | otherwise  = string defAttr "(" <|>
                 string statusMsgColor modes <|>
                 string defAttr ") "
  where
    statusMsgColor = withForeColor defAttr red

-- | Render a 'MessageBody' given the sender's sigils and the nicknames to
-- highlight.
bodyImage ::
  RenderMode ->
  MessageRendererParams ->
  MessageBody -> Image
bodyImage rm params body =
  case body of
    IrcBody    irc -> ircLineImage rm params irc
    ErrorBody  txt -> errorImage params txt
    NormalBody txt -> normalImage params txt

-- | Render a 'ZonedTime' as time using quiet attributes
--
-- @
-- 23:15
-- @
timeImage :: Palette -> ZonedTime -> Image
timeImage palette
  = string (view palTime palette)
  . formatTime defaultTimeLocale "%R "

-- | Render a 'ZonedTime' as full date and time user quiet attributes
--
-- @
-- 2016-07-24 23:15:10
-- @
datetimeImage :: Palette -> ZonedTime -> Image
datetimeImage palette
  = string (view palTime palette)
  . formatTime defaultTimeLocale "%F %T "

-- | Level of detail to use when rendering
data RenderMode
  = NormalRender -- ^ only render nicknames
  | DetailedRender -- ^ render full user info

-- | Optionally insert padding on the right of an 'Image' until it has
-- the minimum width.
rightPad :: RenderMode -> Maybe Integer -> Image -> Image
rightPad NormalRender (Just minWidth) i =
  let w = max 0 (fromIntegral minWidth - imageWidth i)
  in i <|> string defAttr (replicate w ' ')
rightPad _ _ i = i

-- | Render a chat message given a rendering mode, the sigils of the user
-- who sent the message, and a list of nicknames to highlight.
ircLineImage ::
  RenderMode ->
  MessageRendererParams ->
  IrcMsg -> Image
ircLineImage rm !rp body =
  let quietAttr = view palMeta pal
      pal     = rendPalette rp
      sigils  = rendUserSigils rp
      myNicks = rendMyNicks rp
      nicks   = rendNicks rp
      detail img =
        case rm of
          NormalRender -> emptyImage
          DetailedRender -> img
  in
  case body of
    Nick old new ->
      detail (string quietAttr "nick ") <|>
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal rm myNicks old <|>
      string defAttr " is now known as " <|>
      coloredIdentifier pal NormalIdentifier myNicks new

    Join nick _chan ->
      string quietAttr "join " <|>
      coloredUserInfo pal rm myNicks nick

    Part nick _chan mbreason ->
      string quietAttr "part " <|>
      coloredUserInfo pal rm myNicks nick <|>
      foldMap (\reason -> string quietAttr " (" <|>
                          parseIrcText reason <|>
                          string quietAttr ")") mbreason

    Quit nick mbreason ->
      string quietAttr "quit "   <|>
      coloredUserInfo pal rm myNicks nick   <|>
      foldMap (\reason -> string quietAttr " (" <|>
                          parseIrcText reason <|>
                          string quietAttr ")") mbreason

    Kick kicker _channel kickee reason ->
      detail (string quietAttr "kick ") <|>
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal rm myNicks kicker <|>
      string defAttr " kicked " <|>
      coloredIdentifier pal NormalIdentifier myNicks kickee <|>
      string defAttr ": " <|>
      parseIrcText reason

    Topic src _dst txt ->
      detail (string quietAttr "tpic ") <|>
      coloredUserInfo pal rm myNicks src <|>
      string defAttr " changed the topic to: " <|>
      parseIrcText txt

    Notice src _dst txt ->
      detail (string quietAttr "note ") <|>
      rightPad rm (rendNickPadding rp)
        (string (view palSigil pal) sigils <|>
         coloredUserInfo pal rm myNicks src) <|>
      string (withForeColor defAttr red) ": " <|>
      parseIrcTextWithNicks pal myNicks nicks txt

    Privmsg src _dst txt ->
      detail (string quietAttr "chat ") <|>
      rightPad rm (rendNickPadding rp)
        (string (view palSigil pal) sigils <|>
         coloredUserInfo pal rm myNicks src) <|>
      string defAttr ": " <|>
      parseIrcTextWithNicks pal myNicks nicks txt

    Ctcp src _dst "ACTION" txt ->
      detail (string quietAttr "actp ") <|>
      string (withForeColor defAttr blue) "* " <|>
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal rm myNicks src <|>
      string defAttr " " <|>
      parseIrcTextWithNicks pal myNicks nicks txt

    CtcpNotice src _dst "ACTION" txt ->
      detail (string quietAttr "actn ") <|>
      string (withForeColor defAttr red) "* " <|>
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal rm myNicks src <|>
      string defAttr " " <|>
      parseIrcTextWithNicks pal myNicks nicks txt

    Ctcp src _dst cmd txt ->
      detail (string quietAttr "ctcp ") <|>
      string (withForeColor defAttr blue) "! " <|>
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal rm myNicks src <|>
      string defAttr " " <|>
      parseIrcText cmd <|>
      separatorImage <|>
      parseIrcText txt

    CtcpNotice src _dst cmd txt ->
      detail (string quietAttr "ctcp ") <|>
      string (withForeColor defAttr red) "! " <|>
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal rm myNicks src <|>
      string defAttr " " <|>
      parseIrcText cmd <|>
      separatorImage <|>
      parseIrcText txt

    Ping params ->
      string defAttr "PING " <|> separatedParams params

    Pong params ->
      string defAttr "PONG " <|> separatedParams params

    Error reason ->
      string (view palError pal) "ERROR " <|>
      parseIrcText reason

    Reply code params ->
      renderReplyCode rm rp code <|>
      char defAttr ' ' <|>
      separatedParams (dropFst params)
      where
        dropFst = case rm of
                    DetailedRender -> id
                    NormalRender   -> drop 1

    UnknownMsg irc ->
      maybe emptyImage (\ui -> coloredUserInfo pal rm myNicks ui <|> char defAttr ' ')
        (view msgPrefix irc) <|>
      text' defAttr (view msgCommand irc) <|>
      char defAttr ' ' <|>
      separatedParams (view msgParams irc)

    Cap cmd args ->
      text' (withForeColor defAttr magenta) (renderCapCmd cmd) <|>
      text' defAttr ": " <|>
      separatedParams args

    Mode nick _chan params ->
      detail (string quietAttr "mode ") <|>
      string (view palSigil pal) sigils <|>
      coloredUserInfo pal rm myNicks nick <|>
      string defAttr " set mode: " <|>
      ircWords params

    Authenticate{} -> string defAttr "AUTHENTICATE ***"
    BatchStart{}   -> string defAttr "BATCH +"
    BatchEnd{}     -> string defAttr "BATCH -"


renderCapCmd :: CapCmd -> Text
renderCapCmd cmd =
  case cmd of
    CapLs   -> "caps available"
    CapList -> "caps active"
    CapAck  -> "caps acknowledged"
    CapNak  -> "caps rejected"
    CapEnd  -> "caps finished" -- server shouldn't send this
    CapReq  -> "caps requested" -- server shouldn't send this

separatorImage :: Image
separatorImage = char (withForeColor defAttr blue) '·'

-- | Process list of 'Text' as individual IRC formatted words
-- separated by a special separator to distinguish parameters
-- from words within parameters.
separatedParams :: [Text] -> Image
separatedParams = horizCat . intersperse separatorImage . map parseIrcText

-- | Process list of 'Text' as individual IRC formatted words
ircWords :: [Text] -> Image
ircWords = horizCat . intersperse (char defAttr ' ') . map parseIrcText

renderReplyCode :: RenderMode -> MessageRendererParams -> ReplyCode -> Image
renderReplyCode rm rp code@(ReplyCode w) =
  case rm of
    DetailedRender -> string attr (show w)
    NormalRender   ->
      rightPad rm (rendNickPadding rp)
        (text' attr (Text.toLower (replyCodeText info))) <|>
      char defAttr ':'
  where
    info = replyCodeInfo code

    color = case replyCodeType info of
              ClientServerReply -> magenta
              CommandReply      -> green
              ErrorReply        -> red
              UnknownReply      -> yellow

    attr = withForeColor defAttr color

data IdentifierColorMode
  = PrivmsgIdentifier -- ^ An identifier in a PRIVMSG
  | NormalIdentifier  -- ^ An identifier somewhere else

-- | Render a nickname in its hash-based color.
coloredIdentifier ::
  Palette             {- ^ color palette      -} ->
  IdentifierColorMode {- ^ draw mode          -} ->
  HashSet Identifier  {- ^ my nicknames       -} ->
  Identifier          {- ^ identifier to draw -} ->
  Image
coloredIdentifier palette icm myNicks ident =
  text' color (idText ident)
  where
    color
      | ident `HashSet.member` myNicks =
          case icm of
            PrivmsgIdentifier -> view palSelfHighlight palette
            NormalIdentifier  -> view palSelf palette

      | otherwise = v Vector.! i

    v = view palNicks palette
    i = hash ident `mod` Vector.length v

-- | Render an a full user. In normal mode only the nickname will be rendered.
-- If detailed mode the full user info including the username and hostname parts
-- will be rendered. The nickname will be colored.
coloredUserInfo ::
  Palette            {- ^ color palette   -} ->
  RenderMode         {- ^ mode            -} ->
  HashSet Identifier {- ^ my nicks        -} ->
  UserInfo           {- ^ userinfo to draw-} ->
  Image
coloredUserInfo palette NormalRender myNicks ui =
  coloredIdentifier palette NormalIdentifier myNicks (userNick ui)
coloredUserInfo palette DetailedRender myNicks !ui =
  horizCat
    [ coloredIdentifier palette NormalIdentifier myNicks (userNick ui)
    , aux '!' (userName ui)
    , aux '@' (userHost ui)
    ]
  where
    quietAttr = view palMeta palette
    aux x xs
      | Text.null xs = emptyImage
      | otherwise    = char quietAttr x <|> text' quietAttr xs

-- | Render an identifier without using colors. This is useful for metadata.
quietIdentifier :: Palette -> Identifier -> Image
quietIdentifier palette ident =
  text' (view palMeta palette) (idText ident)

-- | Parse message text to construct an image. If the text has formatting
-- control characters in it then the text will be rendered according to
-- the formatting codes. Otherwise the nicknames in the message are
-- highlighted.
parseIrcTextWithNicks ::
  Palette ->
  HashSet Identifier {- ^ my nicks    -} ->
  HashSet Identifier {- ^ other nicks -} ->
  Text -> Image
parseIrcTextWithNicks palette myNicks nicks txt
  | Text.any isControl txt = parseIrcText txt
  | otherwise              = highlightNicks palette myNicks nicks txt

-- | Given a list of nicknames and a chat message, this will generate
-- an image where all of the occurrences of those nicknames are colored.
highlightNicks ::
  Palette ->
  HashSet Identifier {- ^ my nicks    -} ->
  HashSet Identifier {- ^ other nicks -} ->
  Text -> Image
highlightNicks palette myNicks nicks txt = horizCat (highlight1 <$> txtParts)
  where
    txtParts = nickSplit txt
    allNicks = HashSet.union myNicks nicks
    highlight1 part
      | HashSet.member partId allNicks = coloredIdentifier palette PrivmsgIdentifier myNicks partId
      | otherwise                      = text' defAttr part
      where
        partId = mkId part

-- | Returns image and identifier to be used when collapsing metadata
-- messages.
metadataImg :: IrcSummary -> Maybe (Image, Identifier, Maybe Identifier)
metadataImg msg =
  case msg of
    QuitSummary who     -> Just (char (withForeColor defAttr red   ) 'x', who, Nothing)
    PartSummary who     -> Just (char (withForeColor defAttr red   ) '-', who, Nothing)
    JoinSummary who     -> Just (char (withForeColor defAttr green ) '+', who, Nothing)
    CtcpSummary who     -> Just (char (withForeColor defAttr white ) 'C', who, Nothing)
    NickSummary old new -> Just (char (withForeColor defAttr yellow) '>', old, Just new)
    _                   -> Nothing

-- | Image used when treating ignored chat messages as metadata
ignoreImage :: Image
ignoreImage = char (withForeColor defAttr yellow) 'I'
