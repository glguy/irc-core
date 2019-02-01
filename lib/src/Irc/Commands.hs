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
  , ircCnotice
  , ircCprivmsg
  , ircInfo
  , ircInvite
  , ircIson
  , ircJoin
  , ircKick
  , ircKill
  , ircKnock
  , ircLinks
  , ircList
  , ircLusers
  , ircMap
  , ircMode
  , ircMotd
  , ircNick
  , ircNotice
  , ircOper
  , ircPart
  , ircPass
  , ircPing
  , ircPong
  , ircPrivmsg
  , ircRules
  , ircQuit
  , ircRemove
  , ircStats
  , ircTime
  , ircTopic
  , ircUser
  , ircUserhost
  , ircWho
  , ircWhois
  , ircWhowas
  , ircVersion

  -- * ZNC support
  , ircZnc

  -- * SASL support
  , ircAuthenticate
  , encodePlainAuthentication
  , encodeExternalAuthentication
  ) where

import           Irc.RawIrcMsg
import           Irc.Identifier
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
  Text {- ^ servername -} ->
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
  Text {- ^ server -} ->
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

-- | USER command
ircUser ::
  Text {- ^ username -} ->
  Bool {- ^ set +w   -} ->
  Bool {- ^ set +i   -} ->
  Text {- ^ realname -} -> RawIrcMsg
ircUser user set_w set_i real = rawIrcMsg "USER" [user, modeTxt, "*", real]
  where
    modeTxt = Text.pack (show mode)
    mode :: Int
    mode = (if set_w then 4 else 0) -- bit 2
         + (if set_i then 8 else 0) -- bit 3

-- | CAP REQ command
ircCapReq ::
  [Text] {- ^ capabilities -} ->
  RawIrcMsg
ircCapReq caps = rawIrcMsg "CAP" ["REQ", Text.unwords caps]

-- | CAP END command
ircCapEnd :: RawIrcMsg
ircCapEnd = rawIrcMsg "CAP" ["END"]

-- | CAP LS command
ircCapLs :: RawIrcMsg
ircCapLs = rawIrcMsg "CAP" ["LS"]

-- | ZNC command
--
-- /specific to ZNC/
ircZnc ::
  [Text] {- ^ parameters -} ->
  RawIrcMsg
ircZnc = rawIrcMsg "ZNC"

-- | AUTHENTICATE command
ircAuthenticate ::
  Text {- ^ authentication mechanism -} ->
  RawIrcMsg
ircAuthenticate msg = rawIrcMsg "AUTHENTICATE" [msg]

-- | Encoding of username and password in PLAIN authentication
encodePlainAuthentication ::
  Text {- ^ username -} ->
  Text {- ^ password -} ->
  Text
encodePlainAuthentication user pass
  = Text.decodeUtf8
  $ Enc.encode
  $ Text.encodeUtf8
  $ Text.intercalate "\0" [user,user,pass]

-- | Encoding of username in EXTERNAL authentication
encodeExternalAuthentication ::
  Text {- ^ username -} ->
  Text
encodeExternalAuthentication user
  = Text.decodeUtf8
  $ Enc.encode
  $ Text.encodeUtf8 user
