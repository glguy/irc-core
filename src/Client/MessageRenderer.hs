module Client.MessageRenderer
  ( msgImage
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

msgImage :: LocalTime -> [Char] -> [Char] -> MessageBody -> Image
msgImage when statusMsg modes body =
  timeImage when <|> statusMsgImage statusMsg <|> bodyImage modes body

statusMsgImage :: [Char] -> Image
statusMsgImage modes
  | null modes = emptyImage
  | otherwise  = string defAttr "(" <|>
                 string statusMsgColor modes <|>
                 string defAttr ") "
  where
    statusMsgColor = withForeColor defAttr red

bodyImage :: [Char] -> MessageBody -> Image
bodyImage modes body =
  case body of
    IrcBody irc  -> ircLineImage modes irc
    ErrorBody ex -> string defAttr ("Exception: " ++ show ex)
    ExitBody     -> string defAttr "Thread finished"

timeImage :: LocalTime -> Image
timeImage
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%R "

ircLineImage :: [Char] -> IrcMsg -> Image
ircLineImage modes body =
  case body of
    Nick old new ->
      string (withForeColor defAttr cyan) modes <|>
      quietIdentifier old <|>
      string defAttr " became " <|>
      string (withForeColor defAttr cyan) modes <|>
      quietIdentifier new

    Join nick _chan ->
      char (withForeColor defAttr green) '+' <|>
      quietIdentifier nick

    Part nick _chan ->
      char (withForeColor defAttr red) '-' <|>
      quietIdentifier nick

    Quit nick _reason ->
      char (withForeColor defAttr red) 'x' <|>
      quietIdentifier nick

    Kick kicker _channel kickee reason ->
      string (withForeColor defAttr cyan) modes <|>
      coloredIdentifier kicker <|>
      string defAttr " kicked " <|>
      coloredIdentifier kickee <|>
      string defAttr ": " <|>
      parseIrcText reason

    Topic src _dst txt ->
      coloredIdentifier src <|>
      string defAttr " changed topic to " <|>
      parseIrcText txt

    Notice src _dst txt ->
      string (withForeColor defAttr cyan) modes <|>
      coloredIdentifier src <|>
      string (withForeColor defAttr red) ": " <|>
      parseIrcText txt

    Privmsg src _dst txt ->
      string (withForeColor defAttr cyan) modes <|>
      coloredIdentifier src <|>
      string defAttr ": " <|>
      parseIrcText txt

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
      maybe emptyImage prefixImage (view msgPrefix irc) <|>
      text' defAttr (view msgCommand irc) <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                parseIrcText p | p <- view msgParams irc]

    Cap cmd args ->
      string defAttr (show cmd) <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                text' defAttr a | a <- args]

    Mode nick _chan params ->
      string (withForeColor defAttr cyan) modes <|>
      coloredIdentifier nick <|>
      string defAttr " set mode: " <|>
      horizCat [char (withForeColor defAttr blue) '·' <|>
                text' defAttr m | m <- params]

prefixImage :: UserInfo -> Image
prefixImage userInfo =
  let nick = userNick userInfo in
  coloredIdentifier nick
  <|> string defAttr ": "

coloredIdentifier :: Identifier -> Image
coloredIdentifier ident =
  text' (withForeColor defAttr (identifierColor ident)) (idText ident)

quietIdentifier :: Identifier -> Image
quietIdentifier ident =
  text' (withForeColor defAttr brightBlack) (idText ident)
