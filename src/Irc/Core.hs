{-# LANGUAGE OverloadedStrings #-}
module Irc.Core where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Char
import Data.Time
import Data.Time.Clock.POSIX
import Network.IRC.ByteString.Parser
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

data MsgFromServer
  = RplWelcome  ByteString
  | RplYourHost ByteString
  | RplCreated  ByteString
  | RplMyInfo   ByteString ByteString ByteString ByteString ByteString
  | RplISupport [ByteString]
  | RplYourId ByteString
  | RplMotdStart
  | RplMotd      ByteString
  | RplEndOfMotd
  | RplLuserClient ByteString
  | RplLuserOp     ByteString ByteString
  | RplLuserUnknown ByteString
  | RplLuserChannels ByteString ByteString
  | RplLuserMe ByteString
  | RplLuserAdminMe ByteString
  | RplLuserAdminLoc1 ByteString
  | RplLuserAdminLoc2 ByteString
  | RplLuserAdminEmail ByteString
  | RplLocalUsers      ByteString ByteString
  | RplGlobalUsers      ByteString
  | RplStatsConn      ByteString
  | RplTopic ByteString ByteString
  | RplTopicWhoTime ByteString ByteString UTCTime
  | RplIsOn [ByteString]
  | RplWhoReply ByteString ByteString ByteString ByteString ByteString ByteString ByteString
  | RplEndOfWho ByteString
  | Ping ByteString
  | Notice  (Either UserInfo ServerName) ByteString ByteString
  | Topic (Either UserInfo ServerName) ByteString ByteString
  | PrivMsg (Either UserInfo ServerName) ByteString ByteString
  | RplNameReply ChannelType ByteString [ByteString]
  | RplEndOfNames
  | ExtJoin UserInfo ByteString ByteString ByteString
  | Join UserInfo ByteString
  | Nick UserInfo ByteString
  | Mode (Either UserInfo ServerName) ByteString [ByteString]
  | Quit UserInfo ByteString
  | Cap ByteString ByteString
  | Kick (Either UserInfo ServerName) ByteString ByteString ByteString
  | Part UserInfo ByteString ByteString
  | RplWhoisUser ByteString ByteString ByteString ByteString
  | RplWhoisHost ByteString ByteString
  | RplWhoisServer ByteString ByteString ByteString
  | RplWhoisChannels ByteString ByteString
  | RplWhoisSecure ByteString
  | RplWhoisAccount ByteString ByteString
  | RplWhoisIdle ByteString ByteString ByteString
  | RplWhoisOperator ByteString ByteString
  | RplEndOfWhois ByteString
  | RplWhoWasUser ByteString ByteString ByteString ByteString
  | RplBanList ByteString ByteString ByteString UTCTime
  | RplEndOfBanList ByteString
  | RplEndOfWhoWas ByteString
  | RplQuietList ByteString ByteString ByteString ByteString
  | RplEndOfQuietList ByteString
  | RplChannelModeIs ByteString [ByteString]
  | RplChannelUrl ByteString ByteString
  | RplCreationTime ByteString UTCTime
  | ErrChanOpPrivsNeeded ByteString ByteString
  | ErrNickInUse
  | ErrUnknownCommand ByteString
  | ErrNeedsMoreParams ByteString
  | ErrBadChannelKey ByteString
  | Invite (Either UserInfo ServerName) ByteString
  | RplListStart
  | RplList ByteString ByteString ByteString
  | RplListEnd
  | RplHostHidden ByteString
  | RplYoureOper ByteString
  | ErrNoSuchNick ByteString
  | ErrWasNoSuchNick ByteString
  | Away UserInfo ByteString
  deriving (Read, Show)

data ChannelType = SecretChannel | PrivateChannel | PublicChannel
  deriving (Read, Show)

ircGetLine :: Handle -> IO ByteString
ircGetLine h =
  do b <- BS.hGetLine h
     return $! if not (BS.null b) && BS.last b == fromIntegral (ord '\r') then BS.init b else b

parseIrcMsg :: ByteString -> Either String IRCMsg
parseIrcMsg bs = aux (toIRCMsg bs)
  where
  aux (Fail _ _ e) = Left e
  aux (Partial k ) = aux (k BS.empty)
  aux (Done _ x  ) = Right x

ircMsgToServerMsg :: IRCMsg -> Maybe MsgFromServer
ircMsgToServerMsg ircmsg =
  case msgCmd ircmsg of
    "001" -> Just (RplWelcome (msgTrail ircmsg))
    "002" -> Just (RplYourHost (msgTrail ircmsg))
    "003" -> Just (RplCreated (msgTrail ircmsg))

    "004" ->
      do [_,host,version,umodes,lmodes,cmodes] <- Just (msgParams ircmsg)
         Just (RplMyInfo host version umodes lmodes cmodes)

    "005" ->
      do _:params <- Just (msgParams ircmsg)
         let params'
               | BS.null (msgTrail ircmsg) = params
               | otherwise = params ++ [msgTrail ircmsg]
         Just (RplISupport params')
    "042" ->
      do [_,yourid] <- Just (msgParams ircmsg)
         Just (RplYourId yourid)

    "250" -> Just (RplStatsConn (msgTrail ircmsg))
    "251" -> Just (RplLuserClient (msgTrail ircmsg))

    "252" ->
      do [_,num] <- Just (msgParams ircmsg)
         Just (RplLuserOp num (msgTrail ircmsg))

    "253" -> Just (RplLuserUnknown (msgTrail ircmsg))

    "254" ->
      do [_,num] <- Just (msgParams ircmsg)
         Just (RplLuserChannels num (msgTrail ircmsg))

    "255" -> Just (RplLuserMe (msgTrail ircmsg))
    "256" -> Just (RplLuserAdminMe (msgTrail ircmsg))
    "257" -> Just (RplLuserAdminLoc1 (msgTrail ircmsg))
    "258" -> Just (RplLuserAdminLoc2 (msgTrail ircmsg))
    "259" -> Just (RplLuserAdminEmail (msgTrail ircmsg))

    "265" ->
      do [_,localusers,maxusers] <- Just (msgParams ircmsg)
         Just (RplLocalUsers localusers maxusers)

    "266" -> Just (RplGlobalUsers (msgTrail ircmsg))

    "303" -> Just (RplIsOn (filter (not . BS.null) (BS.split 32 (msgTrail ircmsg))))

    "311" ->
      do [_,nick,user,host,_star] <- Just (msgParams ircmsg)
         Just (RplWhoisUser nick user host (msgTrail ircmsg))

    "312" ->
      do [_,nick,server] <- Just (msgParams ircmsg)
         Just (RplWhoisServer nick server (msgTrail ircmsg))

    "314" ->
      do [_,nick,user,host,_star] <- Just (msgParams ircmsg)
         Just (RplWhoWasUser nick user host (msgTrail ircmsg))

    "319" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (RplWhoisChannels nick (msgTrail ircmsg))

    "313" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (RplWhoisOperator nick (msgTrail ircmsg))

    "315" ->
      do [_,chan] <- Just (msgParams ircmsg)
         Just (RplEndOfWho chan)

    "317" ->
      do [_,nick,idle,signon] <- Just (msgParams ircmsg)
         Just (RplWhoisIdle nick idle signon)

    "318" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (RplEndOfWhois nick)

    "321" -> Just RplListStart

    "322" ->
      do [_,chan,num,topic] <- Just (msgParams ircmsg)
         Just (RplList chan num topic)

    "323" -> Just RplListEnd

    "324" -> 
      do _:chan:modes <- Just (msgParams ircmsg)
         Just (RplChannelModeIs chan modes)

    "328" ->
      do [_,chan] <- Just (msgParams ircmsg)
         Just (RplChannelUrl chan (msgTrail ircmsg))

    "329" ->
      do [_,chan,time] <- Just (msgParams ircmsg)
         Just (RplCreationTime chan (asTimeStamp time))

    "330" ->
      do [_,nick,account] <- Just (msgParams ircmsg)
         Just (RplWhoisAccount nick account)

    "332" ->
      do [_,chan] <- Just (msgParams ircmsg)
         Just (RplTopic chan (msgTrail ircmsg))

    "333" ->
      do [_,chan,who,time] <- Just (msgParams ircmsg)
         Just (RplTopicWhoTime chan who (asTimeStamp time))

    "352" ->
      do [_,chan,user,host,server,account,flags] <- Just (msgParams ircmsg)
         Just (RplWhoReply chan user host server account flags (msgTrail ircmsg)) -- trailing is: <hop> <realname>

    "353" ->
      do [_,ty,chan] <- Just (msgParams ircmsg)
         ty' <- case ty of
                  "=" -> Just PublicChannel
                  "*" -> Just PrivateChannel
                  "@" -> Just SecretChannel
                  _   -> Nothing
         Just (RplNameReply ty' chan (filter (not . BS.null) (BS.split 32 (msgTrail ircmsg))))

    "366" -> Just RplEndOfNames

    "367" ->
      do [_,chan,banned,banner,time] <- Just (msgParams ircmsg)
         Just (RplBanList chan banned banner (asTimeStamp time))

    "368" ->
      do [_,chan] <- Just (msgParams ircmsg)
         Just (RplEndOfBanList chan)

    "369" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (RplEndOfWhoWas nick)

    "375" -> Just RplMotdStart
    "372" -> Just (RplMotd (msgTrail ircmsg))
    "376" -> Just RplEndOfMotd

    "378" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (RplWhoisHost nick (msgTrail ircmsg))

    "381" ->
      do [_] <- Just (msgParams ircmsg)
         Just (RplYoureOper (msgTrail ircmsg))

    "396" ->
      do [_,host] <- Just (msgParams ircmsg)
         Just (RplHostHidden host)

    "401" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (ErrNoSuchNick nick)

    "406" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (ErrWasNoSuchNick nick)

    "421" ->
      do [_,cmd] <- Just (msgParams ircmsg)
         Just (ErrUnknownCommand cmd)

    "433" -> Just ErrNickInUse

    "461" ->
      do [_,cmd] <- Just (msgParams ircmsg)
         Just (ErrNeedsMoreParams cmd)

    "475" ->
      do [_,chan] <- Just (msgParams ircmsg)
         Just (ErrBadChannelKey chan)

    "482" ->
      do [_,chan] <- Just (msgParams ircmsg)
         Just (ErrChanOpPrivsNeeded chan (msgTrail ircmsg))

    "671" ->
      do [_,nick] <- Just (msgParams ircmsg)
         Just (RplWhoisSecure nick)

    "728" ->
      do [_,chan,_mode,banned,banner,time] <- Just (msgParams ircmsg)
         Just (RplQuietList chan banned banner time)

    "729" ->
      do [_,chan,_mode] <- Just (msgParams ircmsg)
         Just (RplEndOfQuietList chan)

    "PING" -> Just (Ping (msgTrail ircmsg))

    "PRIVMSG" ->
      do src <- msgPrefix ircmsg
         [dst] <- Just (msgParams ircmsg)
         Just (PrivMsg src dst (msgTrail ircmsg))

    "NOTICE" ->
      do src <- msgPrefix ircmsg
         [dst] <- Just (msgParams ircmsg)
         Just (Notice src dst (msgTrail ircmsg))

    "TOPIC" ->
      do who <- msgPrefix ircmsg
         [chan] <- Just (msgParams ircmsg)
         Just (Topic who chan (msgTrail ircmsg))

    "JOIN" ->
      do Left who <- msgPrefix ircmsg
         case msgParams ircmsg of
           [chan,account] -> Just (ExtJoin who chan account (msgTrail ircmsg))
           [chan] -> Just (Join who chan)
           []     -> Just (Join who (msgTrail ircmsg))
           _      -> Nothing

    "NICK" ->
      do Left who <- msgPrefix ircmsg
         Just (Nick who (msgTrail ircmsg))

    "MODE" ->
      do who <- msgPrefix ircmsg
         (tgt:modes) <- Just (msgParams ircmsg)
         let modes' | BS.null (msgTrail ircmsg) = modes
                    | otherwise = modes ++ [msgTrail ircmsg]
         Just (Mode who tgt modes')

    "PART" ->
      do Left who <- msgPrefix ircmsg
         [chan] <- Just (msgParams ircmsg)
         Just (Part who chan (msgTrail ircmsg))

    "AWAY" ->
      do Left who <- msgPrefix ircmsg
         Just (Away who (msgTrail ircmsg))

    "QUIT" ->
      do Left who <- msgPrefix ircmsg
         Just (Quit who (msgTrail ircmsg))

    "KICK" ->
      do who <- msgPrefix ircmsg
         [chan,tgt] <- Just (msgParams ircmsg)
         Just (Kick who chan tgt (msgTrail ircmsg))

    "INVITE" ->
      do who <- msgPrefix ircmsg
         [_] <- Just (msgParams ircmsg)
         Just (Invite who (msgTrail ircmsg))

    "CAP" ->
      do [_,cmd] <- Just (msgParams ircmsg)
         Just (Cap cmd (msgTrail ircmsg))

    _ -> Nothing

asTimeStamp :: ByteString -> UTCTime
asTimeStamp b =
  case BS8.readInteger b of
    Just (n,_) -> posixSecondsToUTCTime (fromIntegral n)
    Nothing    -> posixSecondsToUTCTime 0

copyIRCMsg :: IRCMsg -> IRCMsg
copyIRCMsg msg =
  prefix' `seq` params' `seq` trail' `seq`
  IRCMsg { msgPrefix = prefix'
         , msgCmd = msgCmd msg
         , msgParams = params'
         , msgTrail = trail'
         }
  where
  prefix' = case msgPrefix msg of
              Just (Right s) -> Just $! (Right $! BS.copy s)
              Just (Left  u) -> Just $! (Left  $! copyUserInfo u)
              Nothing        -> Nothing
  trail'  = BS.copy (msgTrail msg)
  params' = foldr (\x xs -> ((:) $! BS.copy x) $! xs) [] (msgParams msg)

copyUserInfo :: UserInfo -> UserInfo
copyUserInfo u =
  nick' `seq` host' `seq` name' `seq`
  UserInfo
    { userNick = nick'
    , userHost = host'
    , userName = name'
    }
  where
  nick' = BS.copy (userNick u)
  host' = case userHost u of
            Nothing -> Nothing
            Just h  -> Just $! BS.copy h
  name' = case userName u of
            Nothing -> Nothing
            Just h  -> Just $! BS.copy h
