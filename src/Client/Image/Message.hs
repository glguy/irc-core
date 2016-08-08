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
  , defaultRenderParams
  , msgImage
  , detailedMsgImage
  , metadataImg
  , ignoreImage
  , quietIdentifier
  , coloredUserInfo
  , coloredIdentifier
  ) where

import           Client.IdentifierColors
import           Client.Image.Palette
import           Client.Message
import           Client.MircFormatting
import           Control.Lens
import           Data.Time
import           Graphics.Vty.Image
import           Irc.Codes
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           Data.Char
import qualified Data.HashSet as HashSet
import           Data.List
import qualified Data.Text as Text
import           Data.Text (Text)

-- | Parameters used when rendering messages
data MessageRendererParams = MessageRendererParams
  { rendStatusMsg  :: [Char] -- ^ restricted message sigils
  , rendUserSigils :: [Char] -- ^ sender sigils
  , rendNicks      :: [Identifier] -- ^ nicknames to highlight
  , rendMyNicks    :: [Identifier] -- ^ nicknames to highlight in red
  , rendPalette    :: Palette -- ^ nick color palette
  }

-- | Default 'MessageRenderParams' with no sigils or nicknames specified
defaultRenderParams :: MessageRendererParams
defaultRenderParams = MessageRendererParams
  { rendStatusMsg = ""
  , rendUserSigils = ""
  , rendNicks = []
  , rendMyNicks = []
  , rendPalette = defaultPalette
  }

-- | Construct a message given the time the message was received and its
-- render parameters.
msgImage ::
  ZonedTime {- ^ time of message -} ->
  MessageRendererParams -> MessageBody -> Image
msgImage when params body = horizCat
  [ timeImage when
  , statusMsgImage (rendStatusMsg params)
  , bodyImage NormalRender
        (rendPalette params) (rendUserSigils params) (rendMyNicks params) (rendNicks params)
        body
  ]

-- | Construct a message given the time the message was received and its
-- render parameters using a detailed view.
detailedMsgImage :: ZonedTime -> MessageRendererParams -> MessageBody -> Image
detailedMsgImage when params body = horizCat
  [ datetimeImage when
  , statusMsgImage (rendStatusMsg params)
  , bodyImage DetailedRender
      (rendPalette params) (rendUserSigils params) (rendMyNicks params) (rendNicks params) body
  ]

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
  Palette ->
  [Char] {- ^ sigils -} ->
  [Identifier] {- ^ my nicknames -} ->
  [Identifier] {- ^ nicknames to highlight -} ->
  MessageBody -> Image
bodyImage rm palette modes myNicks nicks body =
  case body of
    IrcBody irc  -> ircLineImage palette rm modes myNicks nicks irc
    ErrorBody ex -> string defAttr ("Exception: " ++ show ex)
    ExitBody     -> string defAttr "Thread finished"

-- | Render a 'ZonedTime' as time using quiet attributes
--
-- @
-- 23:15
-- @
timeImage :: ZonedTime -> Image
timeImage
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%R "

-- | Render a 'ZonedTime' as full date and time user quiet attributes
--
-- @
-- 2016-07-24 23:15:10
-- @
datetimeImage :: ZonedTime -> Image
datetimeImage
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%F %T "

-- | Level of detail to use when rendering
data RenderMode
  = NormalRender -- ^ only render nicknames
  | DetailedRender -- ^ render full user info

-- | The attribute to be used for "quiet" content
quietAttr :: Attr
quietAttr = withForeColor defAttr brightBlack

-- | Render a chat message given a rendering mode, the sigils of the user
-- who sent the message, and a list of nicknames to highlight.
ircLineImage ::
  Palette ->
  RenderMode ->
  [Char]       {- ^ sigils (e.g. \@+) -} ->
  [Identifier] {- ^ my nicknames to highlight -} ->
  [Identifier] {- ^ nicknames to highlight -} ->
  IrcMsg -> Image
ircLineImage palette rm sigils myNicks nicks body =
  let detail img =
        case rm of
          NormalRender -> emptyImage
          DetailedRender -> img
  in
  case body of
    Nick old new ->
      detail (string quietAttr "nick ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks old <|>
      string defAttr " became " <|>
      coloredIdentifier palette myNicks new

    Join nick _chan ->
      string quietAttr "join " <|>
      coloredUserInfo palette rm myNicks nick

    Part nick _chan mbreason ->
      string quietAttr "part " <|>
      coloredUserInfo palette rm myNicks nick <|>
      foldMap (\reason -> string quietAttr " (" <|>
                          parseIrcText reason <|>
                          string quietAttr ")") mbreason

    Quit nick mbreason ->
      string quietAttr "quit "   <|>
      coloredUserInfo palette rm myNicks nick   <|>
      foldMap (\reason -> string quietAttr " (" <|>
                          parseIrcText reason <|>
                          string quietAttr ")") mbreason

    Kick kicker _channel kickee reason ->
      detail (string quietAttr "kick ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks kicker <|>
      string defAttr " kicked " <|>
      coloredIdentifier palette myNicks kickee <|>
      string defAttr ": " <|>
      parseIrcText reason

    Topic src _dst txt ->
      coloredUserInfo palette rm myNicks src <|>
      string defAttr " changed topic to " <|>
      parseIrcText txt

    Notice src _dst txt ->
      detail (string quietAttr "note ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks src <|>
      string (withForeColor defAttr red) ": " <|>
      parseIrcTextWithNicks palette myNicks nicks txt

    Privmsg src _dst txt ->
      detail (string quietAttr "chat ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks src <|>
      string defAttr ": " <|>
      parseIrcTextWithNicks palette myNicks nicks txt

    Ctcp src _dst "ACTION" txt ->
      detail (string quietAttr "actp ") <|>
      string (withForeColor defAttr blue) "* " <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks src <|>
      string defAttr " " <|>
      parseIrcTextWithNicks palette myNicks nicks txt

    CtcpNotice src _dst "ACTION" txt ->
      detail (string quietAttr "actn ") <|>
      string (withForeColor defAttr red) "* " <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks src <|>
      string defAttr " " <|>
      parseIrcTextWithNicks palette myNicks nicks txt

    Ctcp src _dst cmd txt ->
      detail (string quietAttr "ctcp ") <|>
      string (withForeColor defAttr blue) "! " <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks src <|>
      string defAttr " " <|>
      parseIrcText cmd <|>
      separatorImage <|>
      parseIrcText txt

    CtcpNotice src _dst cmd txt ->
      detail (string quietAttr "ctcp ") <|>
      string (withForeColor defAttr red) "! " <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks src <|>
      string defAttr " " <|>
      parseIrcText cmd <|>
      separatorImage <|>
      parseIrcText txt

    Ping params ->
      string defAttr "PING " <|> separatedParams params

    Pong params ->
      string defAttr "PONG " <|> separatedParams params

    Error reason ->
      string (withForeColor defAttr red) "ERROR " <|>
      parseIrcText reason

    Reply code params ->
      renderReplyCode rm code <|>
      char defAttr ' ' <|>
      separatedParams (dropFst params)
      where
        dropFst = case rm of
                    DetailedRender -> id
                    NormalRender   -> drop 1

    UnknownMsg irc ->
      maybe emptyImage (\ui -> coloredUserInfo palette rm myNicks ui <|> char defAttr ' ')
        (view msgPrefix irc) <|>
      text' defAttr (view msgCommand irc) <|>
      char defAttr ' ' <|>
      separatedParams (view msgParams irc)

    Cap cmd args ->
      string defAttr (show cmd) <|>
      char defAttr ' ' <|>
      separatedParams args

    Authenticate{} -> string defAttr "AUTHENTICATE ***"

    Mode nick _chan params ->
      detail (string quietAttr "mode ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo palette rm myNicks nick <|>
      string defAttr " set mode: " <|>
      separatedParams params

separatorImage :: Image
separatorImage = char (withForeColor defAttr blue) '·'

separatedParams :: [Text] -> Image
separatedParams = horizCat . intersperse separatorImage . map parseIrcText

renderReplyCode :: RenderMode -> ReplyCode -> Image
renderReplyCode rm code@(ReplyCode w) =
  case rm of
    DetailedRender -> string attr (show w)
    NormalRender   -> text' attr (Text.toLower (replyCodeText info)) <|>
                      char defAttr ':'
  where
    info = replyCodeInfo code

    color = case replyCodeType info of
              ClientServerReply -> magenta
              CommandReply      -> green
              ErrorReply        -> red
              UnknownReply      -> yellow

    attr = withForeColor defAttr color


-- | Render a nickname in its hash-based color.
coloredIdentifier ::
  Palette ->
  [Identifier] {- ^ my nicknames -} ->
  Identifier ->
  Image
coloredIdentifier palette myNicks ident =
  text' (withForeColor defAttr color) (idText ident)
  where
    color
      | ident `elem` myNicks = red
      | otherwise            = identifierColor palette ident

-- | Render an a full user. In normal mode only the nickname will be rendered.
-- If detailed mode the full user info including the username and hostname parts
-- will be rendered. The nickname will be colored.
coloredUserInfo ::
  Palette ->
  RenderMode ->
  [Identifier] {- ^ my nicks -} ->
  UserInfo -> Image
coloredUserInfo palette NormalRender myNicks ui =
  coloredIdentifier palette myNicks (userNick ui)
coloredUserInfo palette DetailedRender myNicks !ui =
  horizCat
    [ coloredIdentifier palette myNicks (userNick ui)
    , aux '!' (userName ui)
    , aux '@' (userHost ui)
    ]
  where
    aux x xs
      | Text.null xs = emptyImage
      | otherwise    = char defAttr x <|> text' quietAttr xs

-- | Render an identifier without using colors. This is useful for metadata.
quietIdentifier :: Identifier -> Image
quietIdentifier ident =
  text' (withForeColor defAttr brightBlack) (idText ident)

-- | Parse message text to construct an image. If the text has formatting
-- control characters in it then the text will be rendered according to
-- the formatting codes. Otherwise the nicknames in the message are
-- highlighted.
parseIrcTextWithNicks ::
  Palette ->
  [Identifier] {- ^ my nicks -} ->
  [Identifier] {- ^ other nicks -} ->
  Text -> Image
parseIrcTextWithNicks palette myNicks nicks txt
  | Text.any isControl txt = parseIrcText txt
  | otherwise              = highlightNicks palette myNicks nicks txt

-- | Given a list of nicknames and a chat message, this will generate
-- an image where all of the occurrences of those nicknames are colored.
highlightNicks ::
  Palette ->
  [Identifier] {- ^ my nicks -} ->
  [Identifier] {- ^ other nicks -} ->
  Text -> Image
highlightNicks palette myNicks nicks txt = horizCat (highlight1 <$> txtParts)
  where
    nickSet = HashSet.fromList nicks
    txtParts = nickSplit txt
    highlight1 part
      | HashSet.member partId nickSet = coloredIdentifier palette myNicks partId
      | otherwise                     = text' defAttr part
      where
        partId = mkId part

-- | Returns image and identifier to be used when collapsing metadata
-- messages.
metadataImg :: IrcMsg -> Maybe (Image, Maybe Identifier)
metadataImg msg =
  case msg of
    Quit who _   -> Just (char (withForeColor defAttr red  ) 'x', Just (userNick who))
    Part who _ _ -> Just (char (withForeColor defAttr red  ) '-', Just (userNick who))
    Join who _   -> Just (char (withForeColor defAttr green) '+', Just (userNick who))
    Ctcp who _ cmd _ | cmd /= "ACTION"  ->
                    Just (char (withForeColor defAttr white) 'C', Just (userNick who))
    Nick old new -> Just (quietIdentifier (userNick old) <|>
                          char (withForeColor defAttr yellow) '-' <|>
                          quietIdentifier new, Nothing)
    _            -> Nothing

-- | Image used when treating ignored chat messages as metadata
ignoreImage :: Image
ignoreImage = char (withForeColor defAttr yellow) 'I'
