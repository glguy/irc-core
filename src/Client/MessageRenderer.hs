module Client.MessageRenderer
  ( MessageRendererParams(..)
  , RenderMode(..)
  , defaultRenderParams
  , msgImage
  , detailedMsgImage
  , metadataImg
  , quietIdentifier
  , coloredUserInfo
  ) where

import           Client.IdentifierColors
import           Client.Message
import           Client.MircFormatting
import           Control.Lens
import           Data.Time
import           Graphics.Vty.Image
import           Irc.Identifier
import           Irc.Message
import           Irc.RawIrcMsg
import           Irc.UserInfo
import qualified Data.HashSet as HashSet
import           Data.List
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Char

data MessageRendererParams = MessageRendererParams
  { rendStatusMsg  :: [Char]
  , rendUserSigils :: [Char]
  , rendNicks      :: [Identifier]
  }

defaultRenderParams :: MessageRendererParams
defaultRenderParams = MessageRendererParams
  { rendStatusMsg = ""
  , rendUserSigils = ""
  , rendNicks = []
  }

msgImage :: ZonedTime -> MessageRendererParams -> MessageBody -> Image
msgImage when params body = horizCat
  [ timeImage when
  , statusMsgImage (rendStatusMsg params)
  , bodyImage NormalRender (rendUserSigils params) (rendNicks params) body
  ]

detailedMsgImage :: ZonedTime -> MessageRendererParams -> MessageBody -> Image
detailedMsgImage when params body = horizCat
  [ datetimeImage when
  , statusMsgImage (rendStatusMsg params)
  , bodyImage DetailedRender (rendUserSigils params) (rendNicks params) body
  ]

statusMsgImage :: [Char] -> Image
statusMsgImage modes
  | null modes = emptyImage
  | otherwise  = string defAttr "(" <|>
                 string statusMsgColor modes <|>
                 string defAttr ") "
  where
    statusMsgColor = withForeColor defAttr red

bodyImage :: RenderMode -> [Char] -> [Identifier] -> MessageBody -> Image
bodyImage rm modes nicks body =
  case body of
    IrcBody irc  -> ircLineImage rm modes nicks irc
    ErrorBody ex -> string defAttr ("Exception: " ++ show ex)
    ExitBody     -> string defAttr "Thread finished"

timeImage :: ZonedTime -> Image
timeImage
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%R "

datetimeImage :: ZonedTime -> Image
datetimeImage
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%F %T "

data RenderMode = NormalRender | DetailedRender

quietAttr :: Attr
quietAttr = withForeColor defAttr brightBlack

ircLineImage :: RenderMode -> [Char] -> [Identifier] -> IrcMsg -> Image
ircLineImage rm sigils nicks body =
  let detail img =
        case rm of
          NormalRender -> emptyImage
          DetailedRender -> img
  in
  case body of
    Nick old new ->
      detail (string quietAttr "nick ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo rm old <|>
      string defAttr " became " <|>
      coloredIdentifier new

    Join nick _chan ->
      string quietAttr "join " <|>
      coloredUserInfo rm nick

    Part nick _chan mbreason ->
      string quietAttr "part " <|>
      coloredUserInfo rm nick <|>
      foldMap (\reason -> string quietAttr " (" <|>
                          parseIrcText reason <|>
                          string quietAttr ")") mbreason

    Quit nick mbreason ->
      string quietAttr "quit "   <|>
      coloredUserInfo rm nick   <|>
      foldMap (\reason -> string quietAttr " (" <|>
                          parseIrcText reason <|>
                          string quietAttr ")") mbreason

    Kick kicker _channel kickee reason ->
      detail (string quietAttr "kick ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo rm kicker <|>
      string defAttr " kicked " <|>
      coloredIdentifier kickee <|>
      string defAttr ": " <|>
      parseIrcText reason

    Topic src _dst txt ->
      coloredUserInfo rm src <|>
      string defAttr " changed topic to " <|>
      parseIrcText txt

    Notice src _dst txt ->
      detail (string quietAttr "note ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo rm src <|>
      string (withForeColor defAttr red) ": " <|>
      parseIrcTextWithNicks nicks txt

    Privmsg src _dst txt ->
      detail (string quietAttr "chat ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo rm src <|>
      string defAttr ": " <|>
      parseIrcTextWithNicks nicks txt

    Action src _dst txt ->
      detail (string quietAttr "chat ") <|>
      string (withForeColor defAttr blue) "* " <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo rm src <|>
      string defAttr " " <|>
      parseIrcTextWithNicks nicks txt

    Ping params ->
      string defAttr "PING" <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                 parseIrcText p | p <- params]

    Pong params ->
      string defAttr "PONG" <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                 parseIrcText p | p <- params]

    Error reason ->
      string (withForeColor defAttr red) "ERROR " <|>
      parseIrcText reason

    Reply code params ->
      string defAttr (show code) <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                parseIrcText p | p <- params]

    UnknownMsg irc ->
      maybe emptyImage (\ui -> coloredUserInfo rm ui <|> char defAttr ' ')
        (view msgPrefix irc) <|>
      text' defAttr (view msgCommand irc) <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                parseIrcText p | p <- view msgParams irc]

    Cap cmd args ->
      string defAttr (show cmd) <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                text' defAttr a | a <- args]

    Mode nick _chan params ->
      detail (string quietAttr "mode ") <|>
      string (withForeColor defAttr cyan) sigils <|>
      coloredUserInfo rm nick <|>
      string defAttr " set mode: " <|>
      horizCat (intersperse (char (withForeColor defAttr blue) '·')
                            (text' defAttr <$> params))

coloredIdentifier :: Identifier -> Image
coloredIdentifier ident =
  text' (withForeColor defAttr (identifierColor ident)) (idText ident)

coloredUserInfo :: RenderMode -> UserInfo -> Image
coloredUserInfo NormalRender ui = coloredIdentifier (userNick ui)
coloredUserInfo DetailedRender ui = horizCat
  [ coloredIdentifier (userNick ui)
  , foldMap (\user -> char defAttr '!' <|> text' quietAttr user) (userName ui)
  , foldMap (\host -> char defAttr '@' <|> text' quietAttr host) (userHost ui)
  ]

quietIdentifier :: Identifier -> Image
quietIdentifier ident =
  text' (withForeColor defAttr brightBlack) (idText ident)

parseIrcTextWithNicks :: [Identifier] -> Text -> Image
parseIrcTextWithNicks nicks txt
  | Text.any isControl txt = parseIrcText txt
  | otherwise              = highlightNicks nicks txt

highlightNicks :: [Identifier] -> Text -> Image
highlightNicks nicks txt = horizCat (highlight1 <$> txtParts)
  where
    nickSet = HashSet.fromList nicks
    txtParts = nickSplit txt
    highlight1 part
      | HashSet.member partId nickSet = coloredIdentifier partId
      | otherwise                     = text' defAttr part
      where
        partId = mkId part

metadataImg :: IrcMsg -> Maybe (Image, Identifier)
metadataImg msg =
  case msg of
    Quit who _   -> Just (char (withForeColor defAttr red  ) 'x', userNick who)
    Part who _ _ -> Just (char (withForeColor defAttr red  ) '-', userNick who)
    Join who _   -> Just (char (withForeColor defAttr green) '+', userNick who)
    _            -> Nothing
