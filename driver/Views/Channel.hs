{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Views.Channel (channelImage) where

import Control.Lens
import Data.Monoid
import Data.Foldable (toList)
import Data.List (stripPrefix)
import Data.Text (Text)
import Data.Time (UTCTime, formatTime)
import Graphics.Vty.Image
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text

import Irc.Format
import Irc.Message
import Irc.Model
import Irc.Core

import ClientState
import ImageUtils

channelImage :: ClientState -> [Image]
channelImage st
  | view clientDetailView st = detailedImageForState st
  | otherwise                = compressedImageForState st

detailedImageForState :: ClientState -> [Image]
detailedImageForState !st
  = map renderOne
  $ map fst
  $ activeMessages st
  where
  renderOne x =
      timestamp <|>
      string (withForeColor defAttr tyColor) (ty ++ " ") <|>
      statusMsgImage (view mesgStatus x) <|>
      renderFullUsermask (view mesgSender x) <|>
      string (withForeColor defAttr blue) (": ") <|>
      cleanText content
    where
    timestamp
      | view clientTimeView st = renderTimestamp (view mesgStamp x)
      | otherwise              = emptyImage

    (tyColor, ty, content) = case view mesgType x of
       JoinMsgType              -> (green  , "Join", "")
       PartMsgType txt          -> (red    , "Part", txt)
       NickMsgType txt          -> (yellow , "Nick", asUtf8 (idBytes txt))
       QuitMsgType txt          -> (red    , "Quit", txt)
       PrivMsgType txt          -> (blue   , "Priv", txt)
       TopicMsgType txt         -> (yellow , "Topc", txt)
       ActionMsgType txt        -> (blue   , "Actn", txt)
       CtcpRspMsgType cmd txt   -> (yellow , "Ctcp", asUtf8 (cmd <> " " <> txt))
       CtcpReqMsgType cmd txt   -> (yellow , "Ctcp", asUtf8 (cmd <> " " <> txt))
       AwayMsgType txt          -> (yellow , "Away", txt)
       NoticeMsgType txt        -> (blue   , "Note", txt)
       KickMsgType who txt      -> (red    , "Kick", asUtf8 (idBytes who) <> " - " <> txt)
       ErrorMsgType txt         -> (red    , "ErrT", txt)
       ErrMsgType err           -> (red    , "ErrR", Text.pack (show err))
       InviteMsgType            -> (yellow , "Invt", "")
       KnockMsgType             -> (yellow , "Knoc", "")
       CallerIdDeliveredMsgType -> (yellow , "Delv", "")
       CallerIdMsgType          -> (yellow , "Call", "")
       ModeMsgType pol mode arg -> (yellow , "Mode", (if pol then "+" else "-")
                                        <> Text.pack [mode, ' ']
                                        <> asUtf8 arg)

renderTimestamp :: UTCTime -> Image
renderTimestamp
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%H:%M:%S "

renderCompressedTimestamp :: UTCTime -> Image
renderCompressedTimestamp
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "[%H:%M] "

activeMessages :: ClientState -> [(IrcMessage,Image)]
activeMessages st =
  case stripPrefix "/filter " (clientInput st) of
    Nothing -> toList msgs
    Just nick -> filter (nickFilter (BS8.pack nick) . fst) (toList msgs)
  where
  msgs = view (clientMessages . ix (focusedName st) . mlMessages) st
  nickFilter nick msg
    = views mesgSender userNick msg == mkId nick

compressedImageForState :: ClientState -> [Image]
compressedImageForState !st = renderOne (activeMessages st)
  where
  width = view clientWidth st

  ncolors = views clientNickColors length st
  formatNick me nick = identImg (withForeColor defAttr color) nick
    where
    color | me = red
          | otherwise = view clientNickColors st
                     !! mod (nickHash (idDenote nick)) ncolors

  ignores = view clientIgnores st

  renderOne [] = []
  renderOne ((msg,colored):msgs) =
    case mbImg of
      Just img -> (timestamp <|> img) : renderOne msgs
      Nothing  -> renderMeta emptyImage ((msg,colored):msgs)

    where
    timestamp
      | view clientTimeView st = renderCompressedTimestamp (view mesgStamp msg)
      | otherwise              = emptyImage

    nick = views mesgSender userNick msg

    visible = not (view (contains nick) ignores)

    mbImg =
       case view mesgType msg of
         PrivMsgType _ | visible -> Just $
           statusMsgImage (view mesgStatus msg) <|>
           views mesgModes modePrefix msg <|>
           formatNick (view mesgMe msg) nick <|>
           string (withForeColor defAttr blue) (": ") <|>
           colored

         NoticeMsgType _ | visible -> Just $
           statusMsgImage (view mesgStatus msg) <|>
           string (withForeColor defAttr red) "! " <|>
           views mesgModes modePrefix msg <|>
           identImg (withForeColor defAttr red) nick <|>
           string (withForeColor defAttr blue) (": ") <|>
           colored

         ActionMsgType _ | visible -> Just $
           statusMsgImage (view mesgStatus msg) <|>
           string (withForeColor defAttr blue) "* " <|>
           views mesgModes modePrefix msg <|>
           identImg (withForeColor defAttr blue) nick <|>
           char defAttr ' ' <|>
           colored

         CtcpRspMsgType cmd params | visible -> Just $
           string (withForeColor defAttr red) "C " <|>
           views mesgModes modePrefix msg <|>
           identImg (withForeColor defAttr blue) nick <|>
           char defAttr ' ' <|>
           cleanText (asUtf8 cmd) <|>
           char defAttr ' ' <|>
           cleanText (asUtf8 params)

         KickMsgType who reason -> Just $
           views mesgModes modePrefix msg <|>
           formatNick (view mesgMe msg) nick <|>
           string (withForeColor defAttr red) " kicked " <|>
           identImg (withForeColor defAttr yellow) who <|>
           string (withForeColor defAttr blue) (": ") <|>
           cleanText reason

         ErrorMsgType err -> Just $
           string (withForeColor defAttr red) "Error: " <|>
           cleanText err

         ErrMsgType err -> Just $
           string (withForeColor defAttr red) "Error: " <|>
           text' defAttr (errorMessage err)

         InviteMsgType -> Just $
           identImg (withForeColor defAttr green) nick <|>
           text' defAttr " has invited you to join"

         CallerIdDeliveredMsgType -> Just $
           identImg (withForeColor defAttr green) nick <|>
           text' defAttr " has been notified of your message"

         CallerIdMsgType -> Just $
           identImg (withForeColor defAttr green) nick <|>
           text' defAttr " has sent you a message, use /ACCEPT to accept"

         ModeMsgType pol m arg -> Just $
           views mesgModes modePrefix msg <|>
           formatNick (view mesgMe msg) nick <|>
           string (withForeColor defAttr red) " set mode " <|>
           string (withForeColor defAttr white) ((if pol then '+' else '-'):[m,' ']) <|>
           utf8Bytestring' (withForeColor defAttr yellow) arg

         TopicMsgType txt -> Just $
           views mesgModes modePrefix msg <|>
           formatNick (view mesgMe msg) nick <|>
           string (withForeColor defAttr red) " set topic " <|>
           cleanText txt

         AwayMsgType txt -> Just $
           string (withForeColor defAttr red) "A " <|>
           formatNick (view mesgMe msg) nick <|>
           string (withForeColor defAttr red) " is away: " <|>
           cleanText txt

         _ -> Nothing

  filterMeta x
    | view clientMetaView st = [x]
    | otherwise              = []

  renderMeta img [] = filterMeta (cropRight width img)
  renderMeta img ((msg,colored):msgs) =
    let who = views mesgSender userNick msg
        visible = not (view (contains who) ignores)
        metaAttr = withForeColor defAttr brightBlack
    in case view mesgType msg of
         CtcpReqMsgType{} ->
           renderMeta
             (img <|>
              char (withForeColor defAttr brightBlue) 'C' <|>
              identImg metaAttr who <|>
              char defAttr ' ') msgs
         JoinMsgType ->
           renderMeta
             (img <|>
              char (withForeColor defAttr green) '+' <|>
              identImg metaAttr who <|>
              char defAttr ' ') msgs
         PartMsgType{} ->
           renderMeta
             (img <|>
              char (withForeColor defAttr red) '-' <|>
              identImg metaAttr who <|>
              char defAttr ' ') msgs
         QuitMsgType{} ->
           renderMeta
             (img <|>
              char (withForeColor defAttr red) 'x' <|>
              identImg metaAttr who <|>
              char defAttr ' ') msgs
         NickMsgType who' ->
           renderMeta
             (img <|>
              identImg metaAttr who <|>
              char (withForeColor defAttr yellow) '-' <|>
              identImg metaAttr who' <|>
              char defAttr ' ') msgs
         KnockMsgType ->
           renderMeta
             (img <|>
              char (withForeColor defAttr yellow) 'K' <|>
              identImg metaAttr who <|>
              char defAttr ' ') msgs
         _ | not visible ->
           renderMeta
             (img <|>
              char (withForeColor defAttr brightBlack) 'I' <|>
              identImg metaAttr who <|>
              char defAttr ' ') msgs
           | otherwise ->
             filterMeta (cropRight width img)
             ++ renderOne ((msg,colored):msgs)


  conn = view clientConnection st

  prefixes = view (connChanModeTypes . modesPrefixModes) conn

  modePrefix modes =
    string (withForeColor defAttr blue)
    [ prefix | (mode,prefix) <- prefixes, mode `elem` modes]


statusMsgImage :: String -> Image
statusMsgImage status
  | null status = emptyImage
  | otherwise =
           char defAttr '(' <|>
           string (withForeColor defAttr brightRed) status <|>
           string defAttr ") "


errorMessage :: IrcError -> Text
errorMessage e =
  case e of
    ErrCantKillServer         -> "Can't kill server"
    ErrYoureBannedCreep       -> "Banned from server"
    ErrNoOrigin               -> "No origin on PING or PONG"
    ErrErroneousNickname nick -> "Erroneous nickname: " <> asUtf8 nick
    ErrNoNicknameGiven        -> "No nickname given"
    ErrNicknameInUse nick     -> "Nickname in use: " <> asUtf8 (idBytes nick)
    ErrNotRegistered          -> "Not registered"
    ErrNoSuchServer server    -> "No such server: " <> asUtf8 server
    ErrUnknownMode mode       -> "Unknown mode: " <> Text.pack [mode]
    ErrNoPrivileges           -> "No privileges"
    ErrUnknownUmodeFlag mode  -> "Unknown UMODE: " <> Text.pack [mode]
    ErrUnknownCommand cmd     -> "Unknown command: " <> asUtf8 cmd
    ErrNoTextToSend           -> "No text to send"
    ErrNoMotd                 -> "No MOTD"
    ErrNoRecipient            -> "No recipient"
    ErrNoAdminInfo server     -> "No admin info for server: "<> asUtf8 server
    ErrAcceptFull             -> "ACCEPT list is full"
    ErrAcceptExist            -> "Already on ACCEPT list"
    ErrAcceptNot              -> "Not on ACCEPT list"
    ErrNeedMoreParams cmd     -> "Need more parameters: " <> asUtf8 cmd
    ErrAlreadyRegistered      -> "Already registered"
    ErrNoPermForHost          -> "No permission for host"
    ErrPasswordMismatch       -> "Password mismatch"
    ErrUsersDontMatch         -> "Can't change modes for other users"
    ErrHelpNotFound _         -> "Help topic not found"
    ErrBadChanName name       -> "Illegal channel name: " <> asUtf8 name
    ErrNoOperHost             -> "No OPER line for this host"

    ErrNoSuchNick             -> "No such nick"
    ErrWasNoSuchNick          -> "Was no such nick"
    ErrOwnMode                -> "Can't send while +g is set"
    ErrNoNonReg               -> "Messages blocked from unregistered users"
    ErrIsChanService nick     -> "Protected service: " <> asUtf8 (idBytes nick)
    ErrBanNickChange          -> "Can't change kick when banned"
    ErrNickTooFast            -> "Changed nickname too fast"
    ErrUnavailResource        -> "Resource unavailable"
    ErrThrottle               -> "Unable to join due to throttle"
    ErrTooManyChannels        -> "Too many channels joined"
    ErrServicesDown           -> "Services are unavailable"
    ErrUserNotInChannel nick  -> "Not in channel: " <> asUtf8 (idBytes nick)
    ErrNotOnChannel           -> "Must join channel"
    ErrChanOpPrivsNeeded      -> "Channel privileges needed"
    ErrBadChannelKey          -> "Bad channel key"
    ErrBannedFromChan         -> "Unable to join due to ban"
    ErrChannelFull            -> "Channel is full"
    ErrInviteOnlyChan         -> "Invite only channel"
    ErrNoSuchChannel          -> "No such channel"
    ErrCannotSendToChan       -> "Cannot send to channel"
    ErrTooManyTargets         -> "Too many targets"
    ErrBanListFull mode       -> "Ban list full: " <> Text.singleton mode
    ErrUserOnChannel nick     -> "User already on channel: " <> asUtf8 (idBytes nick)
    ErrLinkChannel chan       -> "Forwarded to: " <> asUtf8 (idBytes chan)
    ErrNeedReggedNick         -> "Registered nick required"
    ErrVoiceNeeded            -> "Voice or operator status required"

    ErrKnockOnChan            -> "Attempted to knock joined channel"
    ErrTooManyKnocks          -> "Too many knocks"
    ErrChanOpen               -> "Knock unnecessary"
    ErrTargUmodeG             -> "Message ignored by +g mode"
    ErrNoPrivs priv           -> "Oper privilege required: " <> asUtf8 priv
    ErrMlockRestricted m ms   -> "Mode '" <> Text.singleton m <> "' in locked set \"" <> asUtf8 ms <> "\""
