{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a bridge between the low-level text protocol that
-- IRC uses and the high-level events in the "Irc.Model" module.
module Irc.Core
  ( MsgFromServer(..)
  , ircMsgToServerMsg
  ) where

import Data.ByteString (ByteString)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Irc.Format

-- | 'MsgFromServer' provides a typed view of the various IRC protocol messages.
-- There are more messages defined for IRC (and many of those overlap) than
-- are in common use. Please report a bug if a common message is missing
-- from this type.
data MsgFromServer
  -- 001-099 Client-server connection messages
  = RplWelcome  ByteString -- ^ 001 "Welcome to the Internet Relay Network \<nick\>!\<user\>\@\<host\>"
  | RplYourHost ByteString -- ^ 002 "Your host is \<servername\>, running version \<ver\>"
  | RplCreated  ByteString -- ^ 003 "This server was created \<date\>"
  | RplMyInfo   ByteString ByteString ByteString ByteString ByteString -- ^ 004 servername version available-user-modes available-channel-modes
  | RplISupport [ByteString] -- ^ 005 *(KEY=VALUE)
  | RplSnoMask ByteString -- ^ 008 snomask
  | RplYourId ByteString -- ^ 042 unique-id

  -- 200-399 Command responses
  | RplEndOfStats ByteString -- ^ 219 statsquery
  | RplUmodeIs ByteString [ByteString] -- ^ 221 modes *(params)
  | RplStatsConn ByteString -- ^ 250 connection
  | RplLuserClient ByteString -- ^ 251 "There are \<integer\> users and \<integer\> services on \<integer\> servers"
  | RplLuserOp ByteString -- ^ 252 number-of-ops
  | RplLuserUnknown ByteString -- ^ 253 number-of-unknown
  | RplLuserChannels ByteString -- ^ 254 number-of-channels
  | RplLuserMe ByteString -- ^ 255 "I have \<integer\> clients and \<integer\> servers"
  | RplLuserAdminMe ByteString -- ^ 256 server
  | RplLuserAdminLoc1 ByteString -- ^ 257 admin-info-1
  | RplLuserAdminLoc2 ByteString -- ^ 258 admin-info-2
  | RplLuserAdminEmail ByteString -- ^ 259 admin-email
  | RplLocalUsers ByteString ByteString -- ^ 265 local max
  | RplGlobalUsers ByteString ByteString -- ^ 266 global max

  | RplAway Identifier ByteString -- ^ 301 nick away_message
  | RplUserHost [ByteString] -- ^ 302 *(user hosts)
  | RplIsOn [Identifier] -- ^ 303 *(nick)
  | RplUnAway -- ^ 305
  | RplNowAway -- ^ 306
  | RplWhoisUser Identifier ByteString ByteString ByteString -- ^ 311 nick user host realname
  | RplWhoisServer Identifier ByteString ByteString -- ^ 312 nick server serverinfo
  | RplWhoisOperator Identifier ByteString -- ^ 313 nick "is an IRC operator"
  | RplWhoWasUser Identifier ByteString ByteString ByteString -- ^ 314 nick user host realname
  | RplEndOfWho Identifier -- ^ 315 channel
  | RplWhoisIdle Identifier ByteString UTCTime -- ^ 317 nick idle signon
  | RplEndOfWhois Identifier -- ^ 318 nick
  | RplWhoisChannels Identifier ByteString -- ^ 319 nick channels
  | RplListStart -- ^ 321
  | RplList Identifier Integer ByteString -- ^ 322 channel usercount topic
  | RplListEnd -- ^ 323
  | RplChannelModeIs Identifier ByteString [ByteString] -- ^ 324 channel modes *(params)
  | RplNoTopicSet Identifier -- ^ 331 channel
  | RplTopic Identifier ByteString -- ^ 332 channel topic
  | RplChannelUrl Identifier ByteString -- ^ 328 channel url
  | RplCreationTime Identifier UTCTime -- ^ 329 channel timestamp
  | RplWhoisAccount Identifier ByteString -- ^ 330 nick account
  | RplTopicWhoTime Identifier ByteString UTCTime -- ^ 333 channel nickname timestamp
  | RplInviteList Identifier ByteString ByteString UTCTime -- ^ 346 channel mask who timestamp
  | RplEndOfInviteList Identifier -- ^ 347 channel
  | RplExceptionList Identifier ByteString ByteString UTCTime -- ^ 348 channel mask who timestamp
  | RplEndOfExceptionList Identifier -- ^ 349 channel
  | RplWhoReply Identifier ByteString ByteString ByteString Identifier ByteString ByteString -- ^ 352 channel user host server nick flags txt
  | RplNameReply ChannelType Identifier [ByteString] -- ^ 353 channeltype channel names
  | RplEndOfNames Identifier -- ^ 366 channel
  | RplBanList Identifier ByteString ByteString UTCTime -- ^ 367 channel banned banner timestamp
  | RplEndOfBanList Identifier -- ^ 368 channel
  | RplEndOfWhoWas Identifier -- ^ 369 nick
  | RplMotd ByteString -- ^ 372 line-of-motd
  | RplMotdStart -- ^ 375
  | RplEndOfMotd -- ^ 376
  | RplTime ByteString ByteString -- ^ 391 server "\<string showing server's local time\>"
  | RplInfo ByteString -- ^ 371 info
  | RplEndOfInfo -- ^ 374
  | RplWhoisHost Identifier ByteString -- ^ 378 nick host
  | RplWhoisModes Identifier ByteString [ByteString] -- ^ 379 nick modes *(args)
  | RplYoureOper ByteString -- ^ 381 text
  | RplHostHidden ByteString -- ^ 396 hostname

  -- 400-499 Errors
  | ErrNoSuchNick Identifier -- ^ 401 nickname
  | ErrNoSuchServer ByteString -- ^ 402 server
  | ErrNoSuchChannel Identifier -- ^ 403 channel
  | ErrCannotSendToChan Identifier -- ^ 404 channel
  | ErrTooManyChannels Identifier -- ^ 405 channel
  | ErrWasNoSuchNick Identifier -- ^ 406 nick
  | ErrTooManyTargets Identifier -- ^ 407 target
  | ErrNoSuchService Identifier -- ^ 408 target
  | ErrNoRecipient -- ^ 411
  | ErrNoTextToSend -- ^ 412
  | ErrUnknownCommand ByteString -- ^ 421 command
  | ErrNoMotd -- ^ 422
  | ErrNoAdminInfo -- ^ 423
  | ErrNickInUse -- ^ 433
  | ErrUserNotInChannel Identifier Identifier -- ^ 441 nick channel
  | ErrNotOnChannel Identifier -- ^ 442 channel
  | ErrNeedMoreParams ByteString -- ^ 461 command
  | ErrAlreadyRegistered -- ^ 462
  | ErrNoPermForHost -- ^ 463
  | ErrPasswordMismatch -- ^ 464
  | ErrChannelFull Identifier -- ^ 471 channel
  | ErrUnknownMode Char -- ^ 472 mode
  | ErrInviteOnlyChan Identifier -- ^ 473 channel
  | ErrBannedFromChan Identifier -- ^ 474 channel
  | ErrBadChannelKey Identifier -- ^ 475 channel
  | ErrBadChannelMask Identifier -- ^ 476 channel
  | ErrBanListFull Identifier Char -- ^ 476 channel mode
  | ErrNoPrivileges -- ^ 481
  | ErrChanOpPrivsNeeded Identifier -- ^ 482 channel
  | ErrUnknownUmodeFlag Char -- ^ 501 mode
  | ErrUsersDontMatch -- ^ 502

  -- Random high-numbered stuff
  | RplWhoisSecure Identifier -- ^ 671 nick
  | RplHelpStart ByteString ByteString -- ^ 704 topic text
  | RplHelp      ByteString ByteString -- ^ 705 topic text
  | RplEndOfHelp ByteString -- ^ 706 topic text
  | RplQuietList Identifier Char ByteString ByteString UTCTime -- ^ 728 channel mode mask who timestamp
  | RplEndOfQuietList Identifier Char -- ^ 729 channel mode

  -- SASL stuff
  | RplLoggedIn ByteString -- ^ 900 account
  | RplLoggedOut -- ^ 901
  | RplNickLocked -- ^ 902
  | RplSaslSuccess -- ^ 903
  | RplSaslFail -- ^ 904
  | RplSaslTooLong -- ^ 905
  | RplSaslAborted -- ^ 906
  | RplSaslAlready -- ^ 907
  | RplSaslMechs ByteString -- ^ 908 comma-sep-mechs

  | Away UserInfo ByteString
  | Ping ByteString
  | Notice  UserInfo Identifier ByteString
  | Topic UserInfo Identifier ByteString
  | PrivMsg UserInfo Identifier ByteString
  | ExtJoin UserInfo Identifier (Maybe ByteString) ByteString
  | Join UserInfo Identifier
  | Nick UserInfo Identifier
  | Mode UserInfo Identifier [ByteString]
  | Quit UserInfo ByteString
  | Cap ByteString ByteString
  | Kick UserInfo Identifier Identifier ByteString
  | Part UserInfo Identifier ByteString
  | Invite UserInfo Identifier
  | Error ByteString
  | Authenticate ByteString
  | Account UserInfo (Maybe ByteString)
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

    ("008",[_,snomask,_]) ->
       Just (RplSnoMask (B.tail snomask))

    ("042",[_,yourid,_]) ->
       Just (RplYourId yourid)

    ("219",[_,mode,_]) ->
       Just (RplEndOfStats mode)

    ("221", _:mode:params ) ->
       Just (RplUmodeIs mode params)

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

    ("301",[_,nick,message]) ->
       Just (RplAway (mkId nick) message)

    ("302",[_,txt]) ->
       Just (RplUserHost (filter (not . B.null) (B.split 32 txt)))

    ("303",[_,txt]) ->
       Just (RplIsOn (map mkId (filter (not . B.null) (B.split 32 txt))))

    ("305",[_,_]) ->
       Just RplUnAway

    ("306",[_,_]) ->
       Just RplNowAway

    ("311",[_,nick,user,host,_star,txt]) ->
       Just (RplWhoisUser (mkId nick) user host txt)

    ("312",[_,nick,server,txt]) ->
       Just (RplWhoisServer (mkId nick) server txt)

    ("314",[_,nick,user,host,_star,txt]) ->
       Just (RplWhoWasUser (mkId nick) user host txt)

    ("319",[_,nick,txt]) ->
       Just (RplWhoisChannels (mkId nick) txt)

    ("313",[_,nick,txt]) ->
       Just (RplWhoisOperator (mkId nick) txt)

    ("315",[_,chan,_]) ->
       Just (RplEndOfWho (mkId chan))

    ("317",[_,nick,idle,signon,_txt]) ->
       Just (RplWhoisIdle (mkId nick) idle (asTimeStamp signon))

    ("318",[_,nick,_txt]) ->
       Just (RplEndOfWhois (mkId nick))

    ("321",[_,_,_]) ->
       Just RplListStart

    ("322",[_,chan,num,topic]) ->
       Just (RplList (mkId chan) (asNumber num) topic)

    ("323",[_,_]) ->
       Just RplListEnd

    ("324",_:chan:modes:params) ->
       Just (RplChannelModeIs (mkId chan) modes params)

    ("328",[_,chan,url]) ->
       Just (RplChannelUrl (mkId chan) url)

    ("329",[_,chan,time]) ->
       Just (RplCreationTime (mkId chan) (asTimeStamp time))

    ("330",[_,nick,account,_txt]) ->
       Just (RplWhoisAccount (mkId nick) account)

    ("331",[_,chan,_]) ->
       Just (RplNoTopicSet (mkId chan))

    ("332",[_,chan,txt]) ->
       Just (RplTopic (mkId chan) txt)

    ("333",[_,chan,who,time]) ->
       Just (RplTopicWhoTime (mkId chan) who (asTimeStamp time))

    ("346",[_,chan,mask,who,time]) ->
       Just (RplInviteList (mkId chan) mask who (asTimeStamp time))

    ("347",[_,chan,_txt]) ->
       Just (RplEndOfInviteList (mkId chan))

    ("348",[_,chan,mask,who,time]) ->
       Just (RplExceptionList (mkId chan) mask who (asTimeStamp time))

    ("349",[_,chan,_txt]) ->
       Just (RplEndOfExceptionList (mkId chan))

    ("352",[_,chan,user,host,server,nick,flags,txt]) ->
       Just (RplWhoReply (mkId chan) user host server (mkId nick) flags txt) -- trailing is: <hop> <realname>

    ("353",[_,ty,chan,txt]) ->
      do ty' <- case ty of
                  "=" -> Just PublicChannel
                  "*" -> Just PrivateChannel
                  "@" -> Just SecretChannel
                  _   -> Nothing
         Just (RplNameReply ty' (mkId chan) (filter (not . B.null) (B.split 32 txt)))

    ("366",[_,chan,_]) -> Just (RplEndOfNames (mkId chan))

    ("367",[_,chan,banned,banner,time]) ->
       Just (RplBanList (mkId chan) banned banner (asTimeStamp time))

    ("368",[_,chan,_txt]) ->
       Just (RplEndOfBanList (mkId chan))

    ("369",[_,nick,_]) ->
       Just (RplEndOfWhoWas (mkId nick))

    ("371",[_,txt]) ->
       Just (RplInfo txt)

    ("374",[_,_]) ->
       Just RplEndOfInfo

    ("375",[_,_]) -> Just RplMotdStart
    ("372",[_,txt]) -> Just (RplMotd txt)
    ("376",[_,_]) -> Just RplEndOfMotd

    ("379",_:nick:modes:args) ->
       Just (RplWhoisModes (mkId nick )modes args)

    ("378",[_,nick,txt]) ->
       Just (RplWhoisHost (mkId nick )txt)

    ("381",[_,txt]) ->
         Just (RplYoureOper txt)

    ("391",[_,server,txt]) ->
         Just (RplTime server txt)

    ("396",[_,host,_]) ->
         Just (RplHostHidden host)

    ("401",[_,nick,_]) ->
         Just (ErrNoSuchNick (mkId nick))

    ("402",[_,server,_]) ->
         Just (ErrNoSuchServer server)

    ("403",[_,channel,_]) ->
         Just (ErrNoSuchChannel (mkId channel))

    ("404",[_,channel,_]) ->
         Just (ErrCannotSendToChan (mkId channel))

    ("405",[_,channel,_]) ->
         Just (ErrTooManyChannels (mkId channel))

    ("406",[_,nick,_]) ->
         Just (ErrWasNoSuchNick (mkId nick))

    ("407",[_,target,_]) ->
         Just (ErrTooManyTargets (mkId target))

    ("408",[_,target,_]) ->
         Just (ErrNoSuchService (mkId target))

    ("411",[_,_]) ->
         Just ErrNoRecipient

    ("412",[_,_]) ->
         Just ErrNoTextToSend

    ("421",[_,cmd,_]) ->
         Just (ErrUnknownCommand cmd)

    ("422",[_,_]) ->
         Just ErrNoMotd

    ("423",[_,_,_]) ->
         Just ErrNoAdminInfo

    ("433",[_,_]) -> Just ErrNickInUse

    ("441",[_,nick,chan,_]) ->
         Just (ErrUserNotInChannel (mkId nick) (mkId chan))

    ("442",[_,chan,_]) ->
         Just (ErrNotOnChannel (mkId chan))

    ("461",[_,cmd,_]) ->
         Just (ErrNeedMoreParams cmd)

    ("462",[_,_]) ->
         Just ErrAlreadyRegistered

    ("463",[_,_]) ->
         Just ErrNoPermForHost

    ("464",[_,_]) ->
         Just ErrPasswordMismatch

    ("471",[_,chan,_]) ->
         Just (ErrChannelFull (mkId chan))

    ("472",[_,mode,_]) ->
         Just (ErrUnknownMode (B8.head mode))

    ("473",[_,chan,_]) ->
         Just (ErrInviteOnlyChan (mkId chan))

    ("474",[_,chan,_]) ->
         Just (ErrBannedFromChan (mkId chan))

    ("475",[_,chan,_]) ->
         Just (ErrBadChannelKey (mkId chan))

    ("476",[_,chan,_]) ->
         Just (ErrBadChannelMask (mkId chan))

    ("478",[_,chan,mode,_]) ->
         Just (ErrBanListFull (mkId chan) (B8.head mode))

    ("481",[_,_]) ->
         Just ErrNoPrivileges

    ("482",[_,chan,_]) ->
         Just (ErrChanOpPrivsNeeded (mkId chan))

    ("501",[_,mode,_]) ->
         Just (ErrUnknownUmodeFlag (B8.head mode))

    ("502",[_,_]) ->
         Just ErrUsersDontMatch

    ("671",[_,nick,_]) ->
         Just (RplWhoisSecure (mkId nick))

    ("704",[_,topic,txt]) ->
         Just (RplHelpStart topic txt)

    ("705",[_,topic,txt]) ->
         Just (RplHelp topic txt)

    ("706",[_,topic,_]) ->
         Just (RplEndOfHelp topic)

    ("728",[_,chan,mode,banned,banner,time]) ->
         Just (RplQuietList (mkId chan) (B8.head mode) banned banner (asTimeStamp time))

    ("729",[_,chan,mode,_]) ->
         Just (RplEndOfQuietList (mkId chan) (B8.head mode))

    ("900",[_,_,account,_]) ->
         Just (RplLoggedIn account)

    ("901",[_,_,_]) ->
         Just RplLoggedOut

    ("902",[_,_]) ->
         Just RplNickLocked

    ("903",[_,_]) ->
         Just RplSaslSuccess

    ("904",[_,_]) ->
         Just RplSaslFail

    ("905",[_,_]) ->
         Just RplSaslTooLong

    ("906",[_,_]) ->
         Just RplSaslAborted

    ("907",[_,_]) ->
         Just RplSaslAlready

    ("908",[_,mechs,_]) ->
         Just (RplSaslMechs mechs)

    ("PING",[txt]) -> Just (Ping txt)

    ("PRIVMSG",[dst,txt]) ->
      do src <- msgPrefix ircmsg
         Just (PrivMsg src (mkId dst) txt)

    ("NOTICE",[dst,txt]) ->
      do src <- msgPrefix ircmsg
         Just (Notice src (mkId dst) txt)

    ("TOPIC",[chan,txt]) ->
      do who <- msgPrefix ircmsg
         Just (Topic who (mkId chan) txt)

    ("JOIN",[chan,account,real]) ->
      do who <- msgPrefix ircmsg
         Just (ExtJoin who (mkId chan) (if account == "*" then Nothing else Just account) real)

    ("JOIN",[chan]) ->
      do who <- msgPrefix ircmsg
         Just (Join who (mkId chan))

    ("NICK",[newnick]) ->
      do who <- msgPrefix ircmsg
         Just (Nick who (mkId newnick))

    ("MODE",tgt:modes) ->
      do who <- msgPrefix ircmsg
         Just (Mode who (mkId tgt) modes)

    ("PART",[chan]) ->
      do who <- msgPrefix ircmsg
         Just (Part who (mkId chan) "")

    ("PART",[chan,txt]) ->
      do who <- msgPrefix ircmsg
         Just (Part who (mkId chan) txt)

    ("AWAY",[txt]) ->
      do who <- msgPrefix ircmsg
         Just (Away who txt)

    ("QUIT",[txt]) ->
      do who <- msgPrefix ircmsg
         Just (Quit who txt)

    ("KICK",[chan,tgt,txt]) ->
      do who <- msgPrefix ircmsg
         Just (Kick who (mkId chan) (mkId tgt) txt)

    ("INVITE",[_,chan]) ->
      do who <- msgPrefix ircmsg
         Just (Invite who (mkId chan))

    ("CAP",[_,cmd,txt]) ->
         Just (Cap cmd txt)

    ("ERROR",[txt]) ->
         Just (Error txt)

    ("AUTHENTICATE",[txt]) ->
         Just (Authenticate txt)

    ("ACCOUNT",[acct]) ->
      do who <- msgPrefix ircmsg
         Just (Account who (if acct == "*" then Nothing else Just acct))

    _ -> Nothing

asTimeStamp :: ByteString -> UTCTime
asTimeStamp = posixSecondsToUTCTime . fromInteger . asNumber

asNumber :: ByteString -> Integer
asNumber b =
  case B8.readInteger b of
    Nothing    -> 0
    Just (x,_) -> x
