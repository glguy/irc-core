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
  = RplWelcome  ByteString -- ^ 001 "Welcome to the Internet Relay Network \<nick\>!\<user\>\@\<host\>"
  | RplYourHost ByteString -- ^ 002 "Your host is \<servername\>, running version \<ver\>"
  | RplCreated  ByteString -- ^ 003 "This server was created \<date\>"
  | RplMyInfo   ByteString ByteString ByteString ByteString ByteString -- ^ 004 servername version available-user-modes available-channel-modes
  | RplISupport [ByteString] -- ^ 005 *(KEY=VALUE)
  | RplYourId ByteString -- ^ 042 unique-id
  | RplLuserClient ByteString -- ^ 251 "There are \<integer\> users and \<integer\> services on \<integer\> servers"
  | RplLuserOp ByteString -- ^ 252 number-of-ops
  | RplLuserUnknown ByteString -- ^ 253 number-of-unknown
  | RplLuserChannels ByteString -- ^ 254 number-of-channels
  | RplLuserMe ByteString -- ^ 255 "I have \<integer\> clients and \<integer\> servers"
  | RplLuserAdminMe ByteString -- ^ 256 server
  | RplLuserAdminLoc1 ByteString -- ^ 257 admin-info-1
  | RplLuserAdminLoc2 ByteString -- ^ 258 admin-info-2
  | RplLuserAdminEmail ByteString -- ^ 259 admin-email
  | RplMotd ByteString -- ^ 372 line-of-motd
  | RplMotdStart -- ^ 375
  | RplEndOfMotd -- ^ 376
  | RplLocalUsers ByteString ByteString
  | RplGlobalUsers ByteString ByteString
  | RplStatsConn ByteString
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
  | Invite UserInfo ByteString
  | RplListStart
  | RplList ByteString ByteString ByteString
  | RplListEnd
  | RplHostHidden ByteString
  | RplYoureOper ByteString
  | RplInfo ByteString
  | RplEndOfInfo
  | ErrNoSuchNick ByteString -- ^ 401 nickname
  | ErrNoSuchServer ByteString -- ^ 402 server
  | ErrNoSuchChannel ByteString -- ^ 403 channel
  | ErrCannotSendToChan ByteString -- ^ 404 channel
  | ErrTooManyChannels ByteString -- ^ 405 channel
  | ErrWasNoSuchNick ByteString -- ^ 406 nick
  | ErrTooManyTargets ByteString ByteString -- ^ 407 target "\<error code\> recipients. \<abort message\>"
  | ErrNoSuchService ByteString -- ^ 408 target
  | ErrNoRecipient ByteString -- ^ 411 "No recipient given (\<command\>)"
  | ErrNoTextToSend -- ^ 412
  | ErrUnknownCommand ByteString -- ^ 421 command
  | ErrNoMotd -- ^ 422
  | ErrNoAdminInfo ByteString -- ^ 423 server
  | ErrNickInUse -- ^ 433
  | ErrNeedMoreParams ByteString -- ^ 461 command
  | ErrAlreadyRegistered -- ^ 462
  | ErrNoPermForHost -- ^ 463
  | ErrPasswordMismatch -- ^ 464
  | ErrBadChannelKey ByteString -- ^ 475 channel
  | ErrBadChannelMask ByteString -- ^ 476 channel
  | ErrBanListFull ByteString ByteString -- ^ 476 channel mode
  | ErrNoPrivileges -- ^ 481
  | ErrChanOpPrivsNeeded ByteString -- ^ 482 channel
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

    ("252",[_,num,_]) ->
       Just (RplLuserOp num)

    ("253",[_,num,_]) ->
       Just (RplLuserUnknown num)

    ("254",[_,num,_]) ->
       Just (RplLuserChannels num)

    ("255",[_,txt]) -> Just (RplLuserMe txt)
    ("256",[_,server,_]) -> Just (RplLuserAdminMe server)
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

    ("402",[_,server,_]) ->
         Just (ErrNoSuchServer server)

    ("403",[_,channel,_]) ->
         Just (ErrNoSuchChannel channel)

    ("404",[_,channel,_]) ->
         Just (ErrCannotSendToChan channel)

    ("405",[_,channel,_]) ->
         Just (ErrTooManyChannels channel)

    ("406",[_,nick,_]) ->
         Just (ErrWasNoSuchNick nick)

    ("407",[_,target,txt]) ->
         Just (ErrTooManyTargets target txt)

    ("408",[_,target,_]) ->
         Just (ErrNoSuchService target)

    ("411",[_,txt]) ->
         Just (ErrNoRecipient txt)

    ("412",[_,_]) ->
         Just ErrNoTextToSend

    ("421",[_,cmd,_]) ->
         Just (ErrUnknownCommand cmd)

    ("422",[_,_]) ->
         Just ErrNoMotd

    ("423",[_,server,_]) ->
         Just (ErrNoAdminInfo server)

    ("433",[_,_]) -> Just ErrNickInUse

    ("461",[_,cmd,_]) ->
         Just (ErrNeedMoreParams cmd)

    ("462",[_,_]) ->
         Just ErrAlreadyRegistered

    ("463",[_,_]) ->
         Just ErrNoPermForHost

    ("464",[_,_]) ->
         Just ErrPasswordMismatch

    ("475",[_,chan,_]) ->
         Just (ErrBadChannelKey chan)

    ("476",[_,chan,_]) ->
         Just (ErrBadChannelMask chan)

    ("478",[_,chan,mode,_]) ->
         Just (ErrBanListFull chan mode)

    ("481",[_,_]) ->
         Just ErrNoPrivileges

    ("482",[_,chan,_]) ->
         Just (ErrChanOpPrivsNeeded chan)

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
