{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Views.Channel (channelImage) where

import Control.Lens
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Data.List (stripPrefix, intersperse)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (TimeZone, UTCTime, formatTime, utcToZonedTime)
import Graphics.Vty.Image
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Text.Regex.TDFA

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

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
  zone = view clientTimeZone st
  renderOne x =
      timestamp <|>
      string (withForeColor defAttr tyColor) (ty ++ " ") <|>
      statusMsgImage (view mesgStatus x) <|>
      renderFullUsermask (view mesgSender x) <|>
      string (withForeColor defAttr blue) (": ") <|>
      cleanText content
    where
    timestamp
      | view clientTimeView st = renderTimestamp zone (view mesgStamp x)
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

renderTimestamp :: TimeZone -> UTCTime -> Image
renderTimestamp zone
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "%H:%M:%S "
  . utcToZonedTime zone

renderCompressedTimestamp :: TimeZone -> UTCTime -> Image
renderCompressedTimestamp zone
  = string (withForeColor defAttr brightBlack)
  . formatTime defaultTimeLocale "[%H:%M] "
  . utcToZonedTime zone

activeMessages :: ClientState -> [(IrcMessage,Image)]
activeMessages st =
  case clientInputFilter st of
    FilterNicks nicks -> let nickset = Set.fromList (mkId . BS8.pack <$> nicks)
                         in filter (nicksFilter nickset . fst) (toList msgs)
    FilterBody text -> filter (bodyFilter text . fst) (toList msgs)
    NoFilter        -> toList msgs
  where
  msgs = view (clientMessages . ix (focusedName st) . mlMessages) st
  nicksFilter nickset msg
    = views mesgSender userNick msg `Set.member` nickset

  bodyFilter :: String -> IrcMessage -> Bool
  bodyFilter re msg
    = fromMaybe False (textOfMessage msg =~~ re)

textOfMessage :: IrcMessage -> String
textOfMessage mesg =
    let f n = (BS8.unpack $ idBytes $ views mesgSender userNick mesg) <> ": " <> Text.unpack n
    in f (case mesg ^. mesgType of
             PrivMsgType   t -> t
             NoticeMsgType t -> t
             ActionMsgType t -> t
             KickMsgType _ t -> t
             PartMsgType   t -> t
             QuitMsgType   t -> t
             TopicMsgType  t -> t
             ErrorMsgType  t -> t
             _               -> "")

data InputFilter = FilterNicks [String] | FilterBody String | NoFilter

clientInputFilter :: ClientState -> InputFilter
clientInputFilter st = go (clientInput st)
 where
     go (splitAt 8 -> ("/filter ",nicks)) = FilterNicks (words nicks)
     go (splitAt 6 -> ("/grep ",   txt)) = FilterBody txt
     go  _                               = NoFilter

compressedImageForState :: ClientState -> [Image]
compressedImageForState !st = renderOne (activeMessages st)
  where
  zone = view clientTimeZone st
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
      Nothing  -> renderMeta ((msg,colored):msgs)

    where
    timestamp
      | view clientTimeView st = renderCompressedTimestamp zone (view mesgStamp msg)
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

  renderMeta msgs = filterMeta (cropRight width img)
                 ++ renderOne rest
    where
    (mds,rest) = splitWith (processMeta . fst) msgs
    mds1 = mergeMetadatas mds
    img = horizCat (intersperse gap (map renderCompressed mds1))
    gap = char defAttr ' '

  processMeta msg =
    case view mesgType msg of
      CtcpReqMsgType{} -> Just $ SimpleMetadata (char (withForeColor defAttr brightBlue) 'C') who
      JoinMsgType      -> Just $ SimpleMetadata (char (withForeColor defAttr green) '+') who
      PartMsgType{}    -> Just $ SimpleMetadata (char (withForeColor defAttr red) '-') who
      QuitMsgType{}    -> Just $ SimpleMetadata (char (withForeColor defAttr red) 'x') who
      KnockMsgType     -> Just $ SimpleMetadata (char (withForeColor defAttr yellow) 'K') who
      NickMsgType who' -> Just $ NickChange who who'
      _ | not visible  -> Just $ SimpleMetadata (char (withForeColor defAttr yellow) 'I') who
        | otherwise    -> Nothing
    where
    who = views mesgSender userNick msg
    visible = not (view (contains who) ignores)

  conn = view (clientServer0 . ccConnection) st

  prefixes = view (connChanModeTypes . modesPrefixModes) conn

  modePrefix modes =
    string (withForeColor defAttr blue)
    [ prefix | (mode,prefix) <- prefixes, mode `elem` modes]

data CompressedMetadata
  = SimpleMetadata Image Identifier
  | NickChange Identifier Identifier

renderCompressed :: CompressedMetadata -> Image
renderCompressed md =
  case md of
    SimpleMetadata img who -> img <|> identImg metaAttr who
    NickChange who who' ->
      identImg metaAttr who <|>
      char (withForeColor defAttr yellow) '-' <|>
      identImg metaAttr who'
  where
  metaAttr = withForeColor defAttr brightBlack

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

splitWith :: (a -> Maybe b) -> [a] -> ([b],[a])
splitWith f [] = ([],[])
splitWith f (x:xs) =
  case f x of
    Nothing -> ([],x:xs)
    Just y  -> case splitWith f xs of
                 (ys,xs') -> (y:ys, xs')

mergeMetadatas :: [CompressedMetadata] -> [CompressedMetadata]
mergeMetadatas (SimpleMetadata img1 who1 : SimpleMetadata img2 who2 : xs)
  | who1 == who2      = mergeMetadatas (SimpleMetadata (img1 <|> img2) who1 : xs)
mergeMetadatas (x:xs) = x : mergeMetadatas xs
mergeMetadatas []     = []
