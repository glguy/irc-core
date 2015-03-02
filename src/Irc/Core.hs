{-# LANGUAGE OverloadedStrings #-}
module Irc.Core where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Char
import Data.Time
import Data.Time.Clock.POSIX
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Irc.Format

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
  | RplGlobalUsers ByteString ByteString
  | RplStatsConn      ByteString
  | RplTopic ByteString ByteString
  | RplTopicWhoTime ByteString ByteString UTCTime
  | RplNoTopicSet ByteString
  | RplIsOn [ByteString]
  | RplWhoReply ByteString ByteString ByteString ByteString ByteString ByteString ByteString
  | RplEndOfWho ByteString
  | Ping ByteString
  | Notice  UserInfo ByteString ByteString
  | Topic UserInfo ByteString ByteString
  | PrivMsg UserInfo ByteString ByteString
  | RplNameReply ChannelType ByteString [ByteString]
  | RplEndOfNames
  | ExtJoin UserInfo ByteString ByteString ByteString
  | Join UserInfo ByteString
  | Nick UserInfo ByteString
  | Mode UserInfo ByteString [ByteString]
  | Quit UserInfo ByteString
  | Cap ByteString ByteString
  | Kick UserInfo ByteString ByteString ByteString
  | Part UserInfo ByteString ByteString
  | RplWhoisUser ByteString ByteString ByteString ByteString
  | RplWhoisHost ByteString ByteString
  | RplWhoisServer ByteString ByteString ByteString
  | RplWhoisChannels ByteString ByteString
  | RplWhoisSecure ByteString
  | RplWhoisAccount ByteString ByteString
  | RplWhoisIdle ByteString ByteString ByteString
  | RplWhoisOperator ByteString ByteString
  | RplWhoisModes ByteString ByteString
  | RplEndOfWhois ByteString
  | RplWhoWasUser ByteString ByteString ByteString ByteString
  | RplBanList ByteString ByteString ByteString UTCTime
  | RplEndOfBanList ByteString
  | RplInviteList ByteString ByteString ByteString UTCTime
  | RplEndOfInviteList ByteString
  | RplExceptionList ByteString ByteString ByteString UTCTime
  | RplEndOfExceptionList ByteString
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
  | ErrBadChannelKey ByteString ByteString
  | Invite UserInfo ByteString
  | RplListStart
  | RplList ByteString ByteString ByteString
  | RplListEnd
  | RplHostHidden ByteString
  | RplYoureOper ByteString
  | RplInfo ByteString
  | RplEndOfInfo
  | ErrNoSuchNick ByteString
  | ErrWasNoSuchNick ByteString
  | ErrNoTextToSend
  | Away UserInfo ByteString
  deriving (Read, Show)

data ChannelType = SecretChannel | PrivateChannel | PublicChannel
  deriving (Read, Show)

ircMsgToServerMsg :: RawIrcMsg -> Maybe MsgFromServer
ircMsgToServerMsg ircmsg =
  case (msgCommand ircmsg, msgParams ircmsg) of
    ("001",[_,txt]) -> Just (RplWelcome txt)
    ("002",[_,txt]) -> Just (RplYourHost txt)
    ("003",[_,txt]) -> Just (RplCreated txt)

    ("004",[_,host,version,umodes,lmodes,cmodes]) ->
       Just (RplMyInfo host version umodes lmodes cmodes)

    ("005",_:params) ->
       Just (RplISupport params)

    ("042",[_,yourid,_]) ->
       Just (RplYourId yourid)

    ("250",[_,stats]) ->
       Just (RplStatsConn stats)

    ("251",[_,stats]) ->
       Just (RplLuserClient stats)

    ("252",[_,num,txt]) ->
       Just (RplLuserOp num txt)

    ("253",[_,txt]) ->
       Just (RplLuserUnknown txt)

    ("254",[_,num,txt]) ->
       Just (RplLuserChannels num txt)

    ("255",[_,txt]) -> Just (RplLuserMe txt)
    ("256",[_,txt]) -> Just (RplLuserAdminMe txt)
    ("257",[_,txt]) -> Just (RplLuserAdminLoc1 txt)
    ("258",[_,txt]) -> Just (RplLuserAdminLoc2 txt)
    ("259",[_,txt]) -> Just (RplLuserAdminEmail txt)

    ("265",[_,localusers,maxusers,_txt]) ->
       Just (RplLocalUsers localusers maxusers)

    ("266",[_,globalusers,maxusers,_txt]) ->
       Just (RplGlobalUsers globalusers maxusers)

    ("303",[_,txt]) ->
       Just (RplIsOn (filter (not . BS.null) (BS.split 32 txt)))

    ("311",[_,nick,user,host,_star,txt]) ->
       Just (RplWhoisUser nick user host txt)

    ("312",[_,nick,server,txt]) ->
       Just (RplWhoisServer nick server txt)

    ("314",[_,nick,user,host,_star,txt]) ->
       Just (RplWhoWasUser nick user host txt)

    ("319",[_,nick,txt]) ->
       Just (RplWhoisChannels nick txt)

    ("313",[_,nick,txt]) ->
       Just (RplWhoisOperator nick txt)

    ("315",[_,chan,_]) ->
       Just (RplEndOfWho chan)

    ("317",[_,nick,idle,signon,_txt]) ->
       Just (RplWhoisIdle nick idle signon)

    ("318",[_,nick,_txt]) ->
       Just (RplEndOfWhois nick)

    ("321",[_]) ->
       Just RplListStart

    ("322",[_,chan,num,topic]) ->
       Just (RplList chan num topic)

    ("323",[]) ->
       Just RplListEnd

    ("324",_:chan:modes) ->
       Just (RplChannelModeIs chan modes)

    ("328",[_,chan,url]) ->
       Just (RplChannelUrl chan url)

    ("329",[_,chan,time]) ->
       Just (RplCreationTime chan (asTimeStamp time))

    ("330",[_,nick,account,_txt]) ->
       Just (RplWhoisAccount nick account)

    ("331",[_,chan,_]) ->
       Just (RplNoTopicSet chan)

    ("332",[_,chan,txt]) ->
       Just (RplTopic chan txt)

    ("333",[_,chan,who,time]) ->
       Just (RplTopicWhoTime chan who (asTimeStamp time))

    ("346",[_,chan,mask,who,time]) ->
       Just (RplInviteList chan mask who (asTimeStamp time))

    ("347",[_,chan,_txt]) ->
       Just (RplEndOfInviteList chan)

    ("348",[_,chan,mask,who,time]) ->
       Just (RplExceptionList chan mask who (asTimeStamp time))

    ("349",[_,chan,_txt]) ->
       Just (RplEndOfExceptionList chan)

    ("352",[_,chan,user,host,server,account,flags,txt]) ->
       Just (RplWhoReply chan user host server account flags txt) -- trailing is: <hop> <realname>

    ("353",[_,ty,chan,txt]) ->
      do ty' <- case ty of
                  "=" -> Just PublicChannel
                  "*" -> Just PrivateChannel
                  "@" -> Just SecretChannel
                  _   -> Nothing
         Just (RplNameReply ty' chan (filter (not . BS.null) (BS.split 32 txt)))

    ("366",[_,_chan,_]) -> Just RplEndOfNames

    ("367",[_,chan,banned,banner,time]) ->
       Just (RplBanList chan banned banner (asTimeStamp time))

    ("368",[_,chan,_txt]) ->
       Just (RplEndOfBanList chan)

    ("369",[_,nick]) ->
       Just (RplEndOfWhoWas nick)

    ("371",[_,txt]) ->
       Just (RplInfo txt)

    ("374",[_,txt]) ->
       Just RplEndOfInfo

    ("375",[_,_]) -> Just RplMotdStart
    ("372",[_,txt]) -> Just (RplMotd txt)
    ("376",[_,_]) -> Just RplEndOfMotd

    ("379",[_,nick,txt]) ->
       Just (RplWhoisModes nick txt)

    ("378",[_,nick,txt]) ->
       Just (RplWhoisHost nick txt)

    ("381",[_,txt]) ->
         Just (RplYoureOper txt)

    ("396",[_,host,txt]) ->
         Just (RplHostHidden host)

    ("401",[_,nick,_]) ->
         Just (ErrNoSuchNick nick)

    ("406",[_,nick,_]) ->
         Just (ErrWasNoSuchNick nick)

    ("412",[_,_]) ->
         Just ErrNoTextToSend

    ("421",[_,cmd,_]) ->
         Just (ErrUnknownCommand cmd)

    ("433",[_,_]) -> Just ErrNickInUse

    ("461",[_,cmd,_]) ->
         Just (ErrNeedsMoreParams cmd)

    ("475",[_,chan,txt]) ->
         Just (ErrBadChannelKey chan txt)

    ("482",[_,chan,txt]) ->
         Just (ErrChanOpPrivsNeeded chan txt)

    ("671",[_,nick,_]) ->
         Just (RplWhoisSecure nick)

    ("728",[_,chan,_mode,banned,banner,time]) ->
         Just (RplQuietList chan banned banner time)

    ("729",[_,chan,_mode,_]) ->
         Just (RplEndOfQuietList chan)

    ("PING",[txt]) -> Just (Ping txt)

    ("PRIVMSG",[dst,txt]) ->
      do src <- msgPrefix ircmsg
         Just (PrivMsg src dst txt)

    ("NOTICE",[dst,txt]) ->
      do src <- msgPrefix ircmsg
         Just (Notice src dst txt)

    ("TOPIC",[chan,txt]) ->
      do who <- msgPrefix ircmsg
         Just (Topic who chan txt)

    ("JOIN",[chan,account,real]) ->
      do who <- msgPrefix ircmsg
         Just (ExtJoin who chan account real)

    ("JOIN",[chan]) ->
      do who <- msgPrefix ircmsg
         Just (Join who chan)

    ("NICK",[newnick]) ->
      do who <- msgPrefix ircmsg
         Just (Nick who newnick)

    ("MODE",tgt:modes) ->
      do who <- msgPrefix ircmsg
         Just (Mode who tgt modes)

    ("PART",[chan,txt]) ->
      do who <- msgPrefix ircmsg
         Just (Part who chan txt)

    ("AWAY",[txt]) ->
      do who <- msgPrefix ircmsg
         Just (Away who txt)

    ("QUIT",[txt]) ->
      do who <- msgPrefix ircmsg
         Just (Quit who txt)

    ("KICK",[chan,tgt,txt]) ->
      do who <- msgPrefix ircmsg
         Just (Kick who chan tgt txt)

    ("INVITE",[_,chan]) ->
      do who <- msgPrefix ircmsg
         [_] <- Just (msgParams ircmsg)
         Just (Invite who chan)

    ("CAP",[_,cmd,txt]) ->
         Just (Cap cmd txt)

    _ -> Nothing

asTimeStamp :: ByteString -> UTCTime
asTimeStamp b =
  case BS8.readInteger b of
    Just (n,_) -> posixSecondsToUTCTime (fromIntegral n)
    Nothing    -> posixSecondsToUTCTime 0
