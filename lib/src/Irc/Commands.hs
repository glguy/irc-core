{-# Language OverloadedStrings #-}
{-|
Module      : Irc.Commands
Description : Smart constructors for "RawIrcMsg"
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides smart constructors for IRC commands.
-}
module Irc.Commands
  ( ircAdmin
  , ircAway
  , ircCapEnd
  , ircCapLs
  , ircCapReq
  , ircChantrace
  , ircCnotice
  , ircCprivmsg
  , ircEtrace
  , ircInfo
  , ircInvite
  , ircIson
  , ircJoin
  , ircKick
  , ircKill
  , ircKline
  , ircKnock
  , ircLinks
  , ircList
  , ircLusers
  , ircMap
  , ircMasktrace
  , ircMode
  , ircMonitor
  , ircMotd
  , ircNick
  , ircNotice
  , ircOper
  , ircPart
  , ircPass
  , ircPing
  , ircPong
  , ircPrivmsg
  , ircQuit
  , ircRemove
  , ircRules
  , ircStats
  , ircTestline
  , ircTestmask
  , ircTime
  , ircTopic
  , ircTrace
  , ircUnkline
  , ircUser
  , ircUserhost
  , ircVersion
  , ircWho
  , ircWhois
  , ircWhowas

  -- * ZNC support
  , ircZnc

  -- * SASL support
  , AuthenticatePayload(..)
  , ircAuthenticate
  , ircAuthenticates
  , encodePlainAuthentication
  , encodeExternalAuthentication
  ) where

import           Irc.RawIrcMsg
import           Irc.Identifier
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Enc

nonempty :: Text -> [Text]
nonempty txt = filter (not . Text.null) [txt]

-- | PRIVMSG command
ircPrivmsg ::
  Text {- ^ target  -} ->
  Text {- ^ message -} ->
  RawIrcMsg
ircPrivmsg who msg = rawIrcMsg "PRIVMSG" [who, msg]

-- | CPRIVMSG command
--
-- > CPRIVMSG <nickname> <channel> :<message>
ircCprivmsg ::
  Text {- ^ nickname -} ->
  Text {- ^ channel  -} ->
  Text {- ^ message  -} ->
  RawIrcMsg
ircCprivmsg nick chan msg = rawIrcMsg "CPRIVMSG" [nick, chan, msg]

-- | CNOTICE command
--
-- > CNOTICE <nickname> <channel> :<message>
ircCnotice ::
  Text {- ^ nickname -} ->
  Text {- ^ channel  -} ->
  Text {- ^ message  -} ->
  RawIrcMsg
ircCnotice nick chan msg = rawIrcMsg "CNOTICE" [nick, chan, msg]

-- | KNOCK command
--
-- > KNOCK <channel> [<message>]
ircKnock ::
  Text {- ^ channel  -} ->
  Text {- ^ message  -} ->
  RawIrcMsg
ircKnock chan msg = rawIrcMsg "KNOCK" (chan : nonempty msg)

-- | NOTICE command
ircNotice ::
  Text {- ^ target  -} ->
  Text {- ^ message -} ->
  RawIrcMsg
ircNotice who msg = rawIrcMsg "NOTICE" [who, msg]

-- | MODE command
ircMode ::
  Identifier {- ^ target     -} ->
  [Text]     {- ^ parameters -} ->
  RawIrcMsg
ircMode tgt params = rawIrcMsg "MODE" (idText tgt : params)

ircMonitor ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircMonitor params = rawIrcMsg "MONITOR" params

-- | WHOIS command
ircWhois ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircWhois = rawIrcMsg "WHOIS"

-- | WHO command
ircWho ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircWho = rawIrcMsg "WHO"

-- | WHOWAS command
ircWhowas ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircWhowas = rawIrcMsg "WHOWAS"

-- | WALLOPS command
ircWallops ::
  Text {- ^ message -} ->
  RawIrcMsg
ircWallops msg = rawIrcMsg "WALLOPS" [msg]

-- | NICK command
ircNick ::
  Text {- ^ nickname -} ->
  RawIrcMsg
ircNick nick = rawIrcMsg "NICK" [nick]

-- | PART command
ircPart ::
  Identifier {- ^ channel -} ->
  Text       {- ^ message -} ->
  RawIrcMsg
ircPart chan msg = rawIrcMsg "PART" (idText chan : nonempty msg)

-- | JOIN command
ircJoin ::
  Text       {- ^ channel -} ->
  Maybe Text {- ^ key     -} ->
  RawIrcMsg
ircJoin chan (Just key) = rawIrcMsg "JOIN" [chan, key]
ircJoin chan Nothing    = rawIrcMsg "JOIN" [chan]

-- | INVITE command
ircInvite ::
  Text       {- ^ nickname -} ->
  Identifier {- ^ channel  -} ->
  RawIrcMsg
ircInvite nick channel = rawIrcMsg "INVITE" [nick, idText channel]

-- | TOPIC command
ircTopic ::
  Identifier {- ^ channel -} ->
  Text       {- ^ topic   -} ->
  RawIrcMsg
ircTopic chan msg = rawIrcMsg "TOPIC" (idText chan : nonempty msg)

-- | KICK command
ircKick ::
  Identifier {- ^ channel  -} ->
  Text       {- ^ nickname -} ->
  Text       {- ^ message  -} ->
  RawIrcMsg
ircKick chan who msg = rawIrcMsg "KICK" (idText chan : who : nonempty msg)

-- | KILL command
ircKill ::
  Text {- ^ client  -} ->
  Text {- ^ message -} ->
  RawIrcMsg
ircKill who msg = rawIrcMsg "KILL" (who : nonempty msg)

-- | KLINE command
ircKline ::
  Text {- ^ minutes -} ->
  Text {- ^ mask    -} ->
  Text {- ^ reason  -} ->
  RawIrcMsg
ircKline minutes mask reason = rawIrcMsg "KLINE" [minutes, mask, reason]

-- | UNKLINE command
ircUnkline ::
  Text {- ^ mask -} ->
  RawIrcMsg
ircUnkline mask = rawIrcMsg "UNKLINE" [mask]

-- | TESTLINE command
ircTestline ::
  Text {- ^ mask -} ->
  RawIrcMsg
ircTestline mask = rawIrcMsg "TESTLINE" [mask]

-- | TESTMASK command
ircTestmask ::
  Text {- ^ mask  -} ->
  Text {- ^ gecos -} ->
  RawIrcMsg
ircTestmask mask gecos = rawIrcMsg "TESTMASK" (mask : nonempty gecos)

-- | MASKTRACE command
ircMasktrace ::
  Text {- ^ mask  -} ->
  Text {- ^ gecos -} ->
  RawIrcMsg
ircMasktrace mask gecos = rawIrcMsg "MASKTRACE" [mask, gecos]

-- | CHANTRACE command
ircChantrace ::
  Text {- ^ channel -} ->
  RawIrcMsg
ircChantrace channel = rawIrcMsg "CHANTRACE" [channel]

-- | ETRACE command
ircEtrace ::
  Text {- ^ argument -} ->
  RawIrcMsg
ircEtrace arg = rawIrcMsg "ETRACE" [arg]

-- | REMOVE command
ircRemove ::
  Identifier {- ^ channel  -} ->
  Text       {- ^ nickname -} ->
  Text       {- ^ message  -} ->
  RawIrcMsg
ircRemove chan who msg = rawIrcMsg "REMOVE" (idText chan : who : nonempty msg)

-- | QUIT command
ircQuit :: Text {- ^ quit message -} -> RawIrcMsg
ircQuit = rawIrcMsg "QUIT" . nonempty

-- | PASS command
ircPass :: Text {- ^ password -} -> RawIrcMsg
ircPass pass = rawIrcMsg "PASS" [pass]

-- | LIST command
ircList ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircList = rawIrcMsg "LIST"

-- | PING command
ircPing ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircPing = rawIrcMsg "PING"

-- | PONG command
ircPong ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircPong = rawIrcMsg "PONG"

-- | ISON command
ircIson ::
  [Text] {- ^ nicknames -} ->
  RawIrcMsg
ircIson nicks = rawIrcMsg "ISON" [Text.unwords nicks]

-- | TIME command
ircTime ::
  Text {- ^ optional servername -} ->
  RawIrcMsg
ircTime = rawIrcMsg "TIME" . nonempty

-- | USERHOST command
ircUserhost ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircUserhost = rawIrcMsg "USERHOST"

-- | USERIP command
ircUserip ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircUserip = rawIrcMsg "USERIP"

-- | USERS command
ircUsers ::
  Text {- ^ optional servername -} ->
  RawIrcMsg
ircUsers = rawIrcMsg "USERS" . nonempty

-- | STATS command
ircStats ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircStats = rawIrcMsg "STATS"

-- | OPER command
ircOper ::
  Text {- ^ username -} ->
  Text {- ^ password -} ->
  RawIrcMsg
ircOper u p = rawIrcMsg "OPER" [u,p]

-- | LINKS command
ircLinks ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircLinks = rawIrcMsg "LINKS"

-- | AWAY command
ircAway ::
  Text {- ^ message -} ->
  RawIrcMsg
ircAway = rawIrcMsg "AWAY" . nonempty

-- | MAP command
ircMap :: RawIrcMsg
ircMap = rawIrcMsg "MAP" []

-- | INFO command
ircInfo :: RawIrcMsg
ircInfo = rawIrcMsg "INFO" []

-- | RULES command
ircRules ::
  Text {- ^ servername -} ->
  RawIrcMsg
ircRules = rawIrcMsg "RULES" . nonempty

-- | VERSION command
ircVersion ::
  Text {- ^ server -} ->
  RawIrcMsg
ircVersion = rawIrcMsg "VERSION" . nonempty

-- | LUSERS command
--
-- > LUSERS [<mask> [<server>]]
ircLusers ::
  [Text] {- ^ params -} ->
  RawIrcMsg
ircLusers = rawIrcMsg "LUSERS"

-- | MOTD command
--
-- > MOTD [<server>]
ircMotd ::
  Text {- ^ server -} ->
  RawIrcMsg
ircMotd = rawIrcMsg "MOTD" . nonempty

-- | ADMIN command
--
-- > ADMIN [<target>]
ircAdmin ::
  Text {- ^ target -} ->
  RawIrcMsg
ircAdmin = rawIrcMsg "ADMIN" . nonempty

-- | TRACE command
--
-- > TRACE [<target>]
ircTrace ::
  [Text] {- ^ params -} ->
  RawIrcMsg
ircTrace = rawIrcMsg "TRACE"

-- | USER command
ircUser ::
  Text {- ^ username -} ->
  Text {- ^ realname -} -> RawIrcMsg
ircUser user real = rawIrcMsg "USER" [user, "0", "*", real]

-- | CAP REQ command
ircCapReq ::
  [Text] {- ^ capabilities -} ->
  RawIrcMsg
ircCapReq caps = rawIrcMsg "CAP" ["REQ", Text.unwords caps]

-- | CAP END command
ircCapEnd :: RawIrcMsg
ircCapEnd = rawIrcMsg "CAP" ["END"]

-- | CAP LS command - support CAP version 3.2
ircCapLs :: RawIrcMsg
ircCapLs = rawIrcMsg "CAP" ["LS", "302"]

-- | ZNC command
--
-- /specific to ZNC/
ircZnc ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircZnc = rawIrcMsg "ZNC"

-- | Payload for 'ircAuthenticates'
newtype AuthenticatePayload = AuthenticatePayload ByteString
  deriving Show

ircAuthenticate ::
  Text {- ^ authentication mechanism -} ->
  RawIrcMsg
ircAuthenticate msg = rawIrcMsg "AUTHENTICATE" [msg]

-- | AUTHENTICATE command generator. Returns a list
-- because AUTHENTICATE has a chunking behavior.
ircAuthenticates ::
  AuthenticatePayload {- ^ authentication payload -} ->
  [RawIrcMsg]
ircAuthenticates (AuthenticatePayload bytes) =
  map (ircAuthenticate . Text.decodeUtf8) (chunks (Enc.encode bytes))
  where
    chunks :: ByteString -> [ByteString]
    chunks b
      | B.null b          = ["+"]
      | B.length b >= 400 = B.take 400 b : chunks (B.drop 400 b)
      | otherwise         = [b]

-- | Encoding of username and password in PLAIN authentication
encodePlainAuthentication ::
  Text {- ^ authorization identity -} ->
  Text {- ^ authentication identity -} ->
  Text {- ^ password -} ->
  AuthenticatePayload
encodePlainAuthentication authz authc pass
  = AuthenticatePayload
  $ Text.encodeUtf8
  $ Text.intercalate "\0" [authz,authc,pass]

-- | Encoding of username in EXTERNAL authentication
encodeExternalAuthentication ::
  Text {- ^ authorization identity -} ->
  AuthenticatePayload
encodeExternalAuthentication authz = AuthenticatePayload (Text.encodeUtf8 authz)
