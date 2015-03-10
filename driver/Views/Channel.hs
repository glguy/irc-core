{-# LANGUAGE OverloadedStrings #-}
module Views.Channel where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Foldable (toList)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, formatTime)
import Graphics.Vty.Image
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text

import Irc.Format
import Irc.Model
import Irc.Core

import ClientState
import ImageUtils

detailedImageForState :: ClientState -> [Image]
detailedImageForState st
  = map renderOne
  $ activeMessages st
  where
  renderOne x =
      renderTimestamp (view mesgStamp x) <|>
      string (withForeColor defAttr blue) (ty ++ " ") <|>
      renderFullUsermask (view mesgSender x) <|>
      string (withForeColor defAttr blue) (": ") <|>
      cleanText content
    where
    (ty, content) = case view mesgType x of
       JoinMsgType -> ("J", "")
       PartMsgType txt -> ("P", txt)
       NickMsgType txt -> ("C", asUtf8 (idBytes txt))
       QuitMsgType txt -> ("Q", txt)
       PrivMsgType txt -> ("M", txt)
       TopicMsgType txt -> ("T", txt)
       ActionMsgType txt -> ("A", txt)
       AwayMsgType txt -> ("Y", txt)
       NoticeMsgType txt -> ("N", txt)
       KickMsgType who txt -> ("K", asUtf8 (idBytes who) <> " - " <> txt)
       ErrorMsgType txt -> ("E", txt)
       ErrMsgType err -> ("E", Text.pack (show err))
       InviteMsgType -> ("I", "")
       KnockMsgType -> ("J", "")
       ModeMsgType pol mode arg -> ("Z", (if pol then "+" else "-")
                                        <> Text.pack [mode, ' ']
                                        <> asUtf8 arg)

renderTimestamp :: UTCTime -> Image
renderTimestamp
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%H:%M:%S "

activeMessages :: ClientState -> [IrcMessage]
activeMessages st =
  case stripPrefix "/filter " (clientInput st) of
    Nothing -> toList msgs
    Just nick -> filter (nickFilter (BS8.pack nick)) (toList msgs)
  where
  msgs = view (clientMessages . ix (focusedName st) . mlMessages) st
  nickFilter nick msg
    = views mesgSender userNick msg == mkId nick

compressedImageForState :: ClientState -> [Image]
compressedImageForState st = renderOne (activeMessages st)
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
  renderOne (msg:msgs) =
    let nick = views mesgSender userNick msg
        visible = not (view (contains nick) ignores)
    in case view mesgType msg of
         PrivMsgType txt | visible ->
            -- modes me who what) =
            (views mesgModes modePrefix msg <|>
             formatNick (view mesgMe msg) nick <|>
             string (withForeColor defAttr blue) (": ") <|>
             cleanText txt) : renderOne msgs

         NoticeMsgType txt | visible ->
            -- modes me who what) =
            (string (withForeColor defAttr red) "! " <|>
             views mesgModes modePrefix msg <|>
             identImg (withForeColor defAttr red) nick <|>
             string (withForeColor defAttr blue) (": ") <|>
             cleanText txt) : renderOne msgs

         ActionMsgType txt | visible ->
            -- modes me who what) =
            (string (withForeColor defAttr blue) "* " <|>
             views mesgModes modePrefix msg <|>
             identImg (withForeColor defAttr blue) nick <|>
             string (withForeColor defAttr blue) (": ") <|>
             cleanText txt) : renderOne msgs

         KickMsgType who reason ->
            (views mesgModes modePrefix msg <|>
             formatNick (view mesgMe msg) nick <|>
             string (withForeColor defAttr red) " kicked " <|>
             identImg (withForeColor defAttr yellow) who <|>
             string (withForeColor defAttr blue) (": ") <|>
             cleanText reason) : renderOne msgs

         ErrorMsgType err ->
            (string (withForeColor defAttr red) "Error: " <|>
             cleanText err) : renderOne msgs

         ErrMsgType err ->
            (string (withForeColor defAttr red) "Error: " <|>
             text' defAttr (errorMessage err)) : renderOne msgs

         InviteMsgType ->
            (string (withForeColor defAttr red) "? " <|>
             formatNick (view mesgMe msg) nick <|>
             text' defAttr " has invited you") : renderOne msgs

         ModeMsgType pol m arg ->
            (views mesgModes modePrefix msg <|>
             formatNick (view mesgMe msg) nick <|>
             string (withForeColor defAttr red) " set mode " <|>
             string (withForeColor defAttr white) ((if pol then '+' else '-'):[m,' ']) <|>
             utf8Bytestring' (withForeColor defAttr yellow) arg) : renderOne msgs

         TopicMsgType txt ->
            (views mesgModes modePrefix msg <|>
             formatNick (view mesgMe msg) nick <|>
             string (withForeColor defAttr red) " set topic " <|>
             cleanText txt) : renderOne msgs

         AwayMsgType txt ->
            (string (withForeColor defAttr red) "A " <|>
             formatNick (view mesgMe msg) nick <|>
             string (withForeColor defAttr red) " is away: " <|>
             cleanText txt) : renderOne msgs

         _ -> renderMeta emptyImage (msg:msgs)

  renderMeta img [] = [cropRight width img]
  renderMeta img (msg:msgs) =
    let who = views mesgSender userNick msg
        visible = not (view (contains who) ignores)
    in case view mesgType msg of
         JoinMsgType ->
           renderMeta
             (img <|>
              char (withForeColor defAttr green) '+' <|>
              identImg defAttr who <|>
              char defAttr ' ') msgs
         PartMsgType{} ->
           renderMeta
             (img <|>
              char (withForeColor defAttr red) '-' <|>
              identImg defAttr who <|>
              char defAttr ' ') msgs
         QuitMsgType{} ->
           renderMeta
             (img <|>
              char (withForeColor defAttr red) 'x' <|>
              identImg defAttr who <|>
              char defAttr ' ') msgs
         NickMsgType who' ->
           renderMeta
             (img <|>
              identImg defAttr who <|>
              char (withForeColor defAttr yellow) '-' <|>
              identImg defAttr who' <|>
              char defAttr ' ') msgs
         KnockMsgType ->
           renderMeta
             (img <|>
              char (withForeColor defAttr yellow) 'K' <|>
              identImg defAttr who <|>
              char defAttr ' ') msgs
         _ | not visible ->
           renderMeta
             (img <|>
              char (withForeColor defAttr brightBlack) 'I' <|>
              identImg defAttr who <|>
              char defAttr ' ') msgs
           | otherwise ->
             cropRight width img : renderOne (msg:msgs)


  conn = view clientConnection st

  prefixes = view (connChanModeTypes . modesPrefixModes) conn

  modePrefix modes =
    string (withForeColor defAttr blue)
           (mapMaybe (`lookup` prefixes) modes)


nickHash :: ByteString -> Int
nickHash n =
  let h1 = B.foldl' (\acc b -> fromIntegral b + 33 * acc) 0 n
  in h1 + (h1 `quot` 32)



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
    ErrUnavailResource        -> "Resource unavailable"
    ErrThrottle               -> "Unable to join due to throttle"
    ErrTooManyChannels        -> "Too many channels joined"
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
