{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a bridge between the low-level text protocol that
-- IRC uses and the high-level events in the "Irc.Model" module.
module Irc.Core
  ( MsgFromServer(..)
  , IrcError(..)
  , ircMsgToServerMsg
  ) where

import Control.Lens (over, _2)
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
  | RplISupport [(ByteString,ByteString)] -- ^ 005 *(KEY=VALUE)
  | RplSnoMask ByteString -- ^ 008 snomask
  | RplYourId ByteString -- ^ 042 unique-id

  -- 200-399 Command responses
  | RplStatsLinkInfo [ByteString] -- ^ 211 arguments
  | RplStatsCommands [ByteString] -- ^ 212 arguments
  | RplStatsCLine [ByteString] -- ^ 213 arguments
  | RplStatsNLine [ByteString] -- ^ 214 arguments
  | RplStatsILine [ByteString] -- ^ 215 arguments
  | RplStatsKLine [ByteString] -- ^ 216 arguments
  | RplStatsQLine [ByteString] -- ^ 217 arguments
  | RplStatsYLine [ByteString] -- ^ 218 arguments
  | RplEndOfStats Char -- ^ 219 mode
  | RplStatsPLine [ByteString] -- ^ 220 arguments
  | RplUmodeIs ByteString [ByteString] -- ^ 221 modes *(params)
  | RplStatsDLine [ByteString] -- ^ 225
  | RplStatsVLine [ByteString] -- ^ 240
  | RplStatsLLine [ByteString] -- ^ 241
  | RplStatsUptime ByteString -- ^ 242
  | RplStatsOLine [ByteString] -- ^ 243
  | RplStatsHLine [ByteString] -- ^ 244
  | RplStatsSLine [ByteString] -- ^ 245
  | RplStatsPing  [ByteString] -- ^ 246
  | RplStatsXLine [ByteString] -- ^ 247
  | RplStatsULine [ByteString] -- ^ 248
  | RplStatsDebug [ByteString] -- ^ 249
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
  | RplLoadTooHigh ByteString -- ^ 263 command
  | RplLocalUsers [ByteString] -- ^ 265 [local] [max] txt
  | RplGlobalUsers [ByteString] -- ^ 266 [global] [max] txt
  | RplWhoisCertFp Identifier ByteString -- ^ 276 nick txt
  | RplAcceptList Identifier -- ^ 281
  | RplEndOfAccept -- ^ 282

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
  | RplInviting Identifier Identifier -- ^ 341 nick channel
  | RplInviteList Identifier ByteString ByteString UTCTime -- ^ 346 channel mask who timestamp
  | RplEndOfInviteList Identifier -- ^ 347 channel
  | RplExceptionList Identifier ByteString ByteString UTCTime -- ^ 348 channel mask who timestamp
  | RplEndOfExceptionList Identifier -- ^ 349 channel
  | RplVersion ByteString ByteString ByteString -- ^ 351 version server comments
  | RplWhoReply Identifier ByteString ByteString ByteString Identifier ByteString ByteString -- ^ 352 channel user host server nick flags txt
  | RplNameReply ChannelType Identifier [ByteString] -- ^ 353 channeltype channel names
  | RplLinks ByteString ByteString ByteString -- ^ 364 mask server info
  | RplEndOfLinks ByteString -- ^ 365 mask
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

  | Err Identifier IrcError

  -- Random high-numbered stuff
  | RplWhoisSecure Identifier -- ^ 671 nick
  | RplHelpStart ByteString ByteString -- ^ 704 topic text
  | RplHelp      ByteString ByteString -- ^ 705 topic text
  | RplEndOfHelp ByteString -- ^ 706 topic text
  | RplKnock Identifier UserInfo -- ^ 710 channel
  | RplKnockDelivered Identifier -- ^ 711 channel
  | RplTargNotify Identifier -- ^ 717 nick
  | RplUmodeGMsg Identifier ByteString -- ^ 718 nick mask
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
  | Pong ByteString (Maybe ByteString)
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

data IrcError
  -- 400-499 Errors
  = ErrNoSuchNick -- ^ 401
  | ErrNoSuchServer ByteString -- ^ 402 server
  | ErrNoSuchChannel -- ^ 403
  | ErrCannotSendToChan -- ^ 404
  | ErrTooManyChannels -- ^ 405
  | ErrWasNoSuchNick -- ^ 406
  | ErrTooManyTargets -- ^ 407
  | ErrNoOrigin -- ^ 409
  | ErrNoRecipient -- ^ 411
  | ErrNoTextToSend -- ^ 412
  | ErrUnknownCommand ByteString -- ^ 421 command
  | ErrNoMotd -- ^ 422
  | ErrNoAdminInfo ByteString -- ^ 423 server
  | ErrNoNicknameGiven -- ^ 431
  | ErrErroneousNickname ByteString -- ^ 432 badnick
  | ErrNicknameInUse Identifier -- ^ 433 nick
  | ErrBanNickChange -- ^ 435
  | ErrUnavailResource -- ^ 437
  | ErrNickTooFast -- ^ 438
  | ErrServicesDown -- ^ 440
  | ErrUserNotInChannel Identifier -- ^ 441 nick
  | ErrNotOnChannel -- ^ 442 channel
  | ErrUserOnChannel Identifier -- ^ 443 nick
  | ErrNotRegistered -- ^ 451
  | ErrAcceptFull -- ^ 456
  | ErrAcceptExist -- ^ 457
  | ErrAcceptNot -- ^ 458
  | ErrNeedMoreParams ByteString -- ^ 461 command
  | ErrAlreadyRegistered -- ^ 462
  | ErrNoPermForHost -- ^ 463
  | ErrPasswordMismatch -- ^ 464
  | ErrYoureBannedCreep -- ^ 465
  | ErrLinkChannel Identifier -- ^ 470 dstchannel
  | ErrChannelFull -- ^ 471 channel
  | ErrUnknownMode Char -- ^ 472 mode
  | ErrInviteOnlyChan -- ^ 473
  | ErrBannedFromChan -- ^ 474
  | ErrBadChannelKey -- ^ 475
  | ErrNeedReggedNick -- ^ 477
  | ErrBanListFull Char -- ^ 478 mode
  | ErrBadChanName ByteString -- ^ 479 name
  | ErrThrottle -- ^ 480
  | ErrNoPrivileges -- ^ 481
  | ErrChanOpPrivsNeeded -- ^ 482
  | ErrCantKillServer -- ^ 483
  | ErrIsChanService Identifier -- ^ 484 nick
  | ErrNoNonReg -- ^ 486
  | ErrVoiceNeeded -- ^ 489
  | ErrNoOperHost -- ^ 491
  | ErrOwnMode -- ^ 494
  | ErrUnknownUmodeFlag Char -- ^ 501 mode
  | ErrUsersDontMatch -- ^ 502
  | ErrHelpNotFound ByteString -- ^ 524 topic
  | ErrTooManyKnocks -- ^ 713
  | ErrChanOpen -- ^ 713
  | ErrKnockOnChan -- ^ 714
  | ErrTargUmodeG -- ^ 716
  | ErrMlockRestricted Char ByteString -- ^ 742 mode setting
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

    ("005",_:params)
      | not (null params) ->
         let parse1 = over _2 (B.drop 1) . B8.break (=='=')
         in Just (RplISupport (map parse1 (init params)))

    ("008",[_,snomask,_]) ->
       Just (RplSnoMask (B.tail snomask))

    ("042",[_,yourid,_]) ->
       Just (RplYourId yourid)

    ("211", _:linkinfo) -> Just (RplStatsLinkInfo linkinfo)
    ("212", _:commands) -> Just (RplStatsCommands commands)
    ("213", _:cline   ) -> Just (RplStatsCLine cline)
    ("214", _:nline   ) -> Just (RplStatsNLine nline)
    ("215", _:iline   ) -> Just (RplStatsILine iline)
    ("216", _:kline   ) -> Just (RplStatsKLine kline)
    ("217", _:qline   ) -> Just (RplStatsQLine qline)
    ("218", _:yline   ) -> Just (RplStatsYLine yline)
    ("219",[_,mode,_] ) -> Just (RplEndOfStats (B8.head mode))
    ("220", _:pline   ) -> Just (RplStatsPLine pline)
    ("221", _:mode:params) -> Just (RplUmodeIs mode params)
    ("225", _:dline   ) -> Just (RplStatsDLine dline)
    ("240", _:vline   ) -> Just (RplStatsVLine vline)
    ("241", _:lline   ) -> Just (RplStatsLLine lline)
    ("242", [_,uptime]) -> Just (RplStatsUptime uptime)
    ("243", _:oline   ) -> Just (RplStatsOLine oline)
    ("244", _:hline   ) -> Just (RplStatsHLine hline)
    ("245", _:sline   ) -> Just (RplStatsSLine sline)
    ("246", _:ping    ) -> Just (RplStatsPing  ping )
    ("247", _:xline   ) -> Just (RplStatsXLine xline)
    ("248", _:uline   ) -> Just (RplStatsULine uline)
    ("249", _:debug   ) -> Just (RplStatsDebug debug)

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

    ("263",[_,cmd,_]) ->
       Just (RplLoadTooHigh cmd)

    ("265", _:params) ->
       Just (RplLocalUsers params)

    ("266", _:params ) ->
       Just (RplGlobalUsers params)

    ("276",[_,nick,txt]) ->
       Just (RplWhoisCertFp (mkId nick) txt)

    ("281",[_,nick]) ->
       Just (RplAcceptList (mkId nick))

    ("282",[_,_]) ->
       Just RplEndOfAccept

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

    ("341",[_,nick,chan,_]) ->
       Just (RplInviting (mkId nick) (mkId chan))

    ("346",[_,chan,mask,who,time]) ->
       Just (RplInviteList (mkId chan) mask who (asTimeStamp time))

    ("347",[_,chan,_txt]) ->
       Just (RplEndOfInviteList (mkId chan))

    ("348",[_,chan,mask,who,time]) ->
       Just (RplExceptionList (mkId chan) mask who (asTimeStamp time))

    ("349",[_,chan,_txt]) ->
       Just (RplEndOfExceptionList (mkId chan))

    ("351",[_,version,server,comments]) ->
       Just (RplVersion version server comments)

    ("352",[_,chan,user,host,server,nick,flags,txt]) ->
       Just (RplWhoReply (mkId chan) user host server (mkId nick) flags txt) -- trailing is: <hop> <realname>

    ("353",[_,ty,chan,txt]) ->
      do ty' <- case ty of
                  "=" -> Just PublicChannel
                  "*" -> Just PrivateChannel
                  "@" -> Just SecretChannel
                  _   -> Nothing
         Just (RplNameReply ty' (mkId chan) (filter (not . B.null) (B.split 32 txt)))

    ("364",[_,mask,server,info]) -> Just (RplLinks mask server info)
    ("365",[_,mask,_]          ) -> Just (RplEndOfLinks mask)

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
         Just (Err (mkId nick) ErrNoSuchNick)

    ("402",[_,server,_]) ->
         Just (Err "" (ErrNoSuchServer server))

    ("403",[_,channel,_]) ->
         Just (Err (mkId channel) ErrNoSuchChannel)

    ("404",[_,channel,_]) ->
         Just (Err (mkId channel) ErrCannotSendToChan)

    ("405",[_,channel,_]) ->
         Just (Err (mkId channel) ErrTooManyChannels)

    ("406",[_,nick,_]) ->
         Just (Err (mkId nick) ErrWasNoSuchNick)

    ("407",[_,target,_]) ->
         Just (Err (mkId target) ErrTooManyTargets)

    ("409",[_,_]) ->
         Just (Err "" ErrNoOrigin)

    ("411",[_,_]) ->
         Just (Err "" ErrNoRecipient)

    ("412",[_,_]) ->
         Just (Err "" ErrNoTextToSend)

    ("421",[_,cmd,_]) ->
         Just (Err "" (ErrUnknownCommand cmd))

    ("422",[_,_]) ->
         Just (Err "" ErrNoMotd)

    ("423",[_,server,_]) ->
         Just (Err "" (ErrNoAdminInfo server))

    ("431",[_,_]) -> Just (Err "" ErrNoNicknameGiven)

    ("432",[_,nick,_]) -> Just (Err "" (ErrErroneousNickname nick))

    ("433",[_,nick,_]) -> Just (Err "" (ErrNicknameInUse (mkId nick)))

    ("435",[_,chan,_]) -> Just (Err (mkId chan) ErrBanNickChange)

    ("437",[_,ident,_]) -> Just (Err (mkId ident) ErrUnavailResource)

    ("438",[_,_,_,_]) -> Just (Err "" ErrNickTooFast)

    ("441",[_,nick,_]) ->
         Just (Err (mkId nick) ErrServicesDown)

    ("441",[_,nick,chan,_]) ->
         Just (Err (mkId chan) (ErrUserNotInChannel (mkId nick)))

    ("442",[_,chan,_]) ->
         Just (Err (mkId chan) ErrNotOnChannel)

    ("443",[_,nick,chan,_]) ->
         Just (Err (mkId chan) (ErrUserOnChannel (mkId nick)))

    ("451",[_,_]) ->
         Just (Err "" ErrNotRegistered)

    ("456",[_,_]) ->
         Just (Err "" ErrAcceptFull)

    ("457",[_,nick,_]) ->
         Just (Err (mkId nick) ErrAcceptExist)

    ("458",[_,nick,_]) ->
         Just (Err (mkId nick) ErrAcceptNot)

    ("461",[_,cmd,_]) ->
         Just (Err "" (ErrNeedMoreParams cmd))

    ("462",[_,_]) ->
         Just (Err "" ErrAlreadyRegistered)

    ("463",[_,_]) ->
         Just (Err "" ErrNoPermForHost)

    ("464",[_,_]) ->
         Just (Err "" ErrPasswordMismatch)

    ("465",[_,_]) ->
         Just (Err "" ErrYoureBannedCreep)

    ("470",[_,chan1,chan2,_]) ->
         Just (Err (mkId chan1) (ErrLinkChannel (mkId chan2)))

    ("471",[_,chan,_]) ->
         Just (Err (mkId chan) ErrChannelFull)

    ("472",[_,mode,_]) ->
         Just (Err "" (ErrUnknownMode (B8.head mode)))

    ("473",[_,chan,_]) ->
         Just (Err (mkId chan) ErrInviteOnlyChan)

    ("474",[_,chan,_]) ->
         Just (Err (mkId chan) ErrBannedFromChan)

    ("475",[_,chan,_]) ->
         Just (Err (mkId chan) ErrBadChannelKey)

    ("477",[_,chan,_]) ->
         Just (Err (mkId chan) ErrNeedReggedNick)

    ("478",[_,chan,mode,_]) ->
         Just (Err (mkId chan) (ErrBanListFull (B8.head mode)))

    ("479",[_,chan,_]) ->
         Just (Err "" (ErrBadChanName chan))

    ("480",[_,chan,_]) ->
         Just (Err (mkId chan) ErrThrottle)

    ("481",[_,_]) ->
         Just (Err "" ErrNoPrivileges)

    ("482",[_,chan,_]) ->
         Just (Err (mkId chan) ErrChanOpPrivsNeeded)

    ("483",[_,_]) ->
         Just (Err "" ErrCantKillServer)

    ("484",[_,nick,chan,_]) ->
         Just (Err (mkId chan) (ErrIsChanService (mkId nick)))

    ("486",[_,nick,_]) ->
         Just (Err (mkId nick) ErrNoNonReg)

    ("489",[_,chan,_]) ->
         Just (Err (mkId chan) ErrVoiceNeeded)

    ("491",[_,_]) ->
         Just (Err "" ErrNoOperHost)

    ("494",[_,nick,_]) ->
         Just (Err (mkId nick) ErrOwnMode)

    ("501",[_,mode,_]) ->
         Just (Err "" (ErrUnknownUmodeFlag (B8.head mode)))

    ("502",[_,_]) ->
         Just (Err "" ErrUsersDontMatch)

    ("524",[_,topic,_]) ->
         Just (Err "" (ErrHelpNotFound topic))

    ("671",[_,nick,_]) ->
         Just (RplWhoisSecure (mkId nick))

    ("704",[_,topic,txt]) ->
         Just (RplHelpStart topic txt)

    ("705",[_,topic,txt]) ->
         Just (RplHelp topic txt)

    ("706",[_,topic,_]) ->
         Just (RplEndOfHelp topic)

    ("710",[_,chan,who,_]) ->
         Just (RplKnock (mkId chan) (parseUserInfo who))

    ("711",[_,chan,_]) ->
         Just (RplKnockDelivered (mkId chan))

    ("712",[_,chan,_]) ->
         Just (Err (mkId chan) ErrTooManyKnocks)

    ("713",[_,chan,_]) ->
         Just (Err (mkId chan) ErrChanOpen)

    ("714",[_,chan,_]) ->
         Just (Err (mkId chan) ErrKnockOnChan)

    ("716",[_,nick,_]) ->
         Just (Err (mkId nick) ErrTargUmodeG)

    ("717",[_,nick,_]) ->
         Just (RplTargNotify (mkId nick))

    ("718",[_,nick,mask,_]) ->
         Just (RplUmodeGMsg (mkId nick) mask)

    ("728",[_,chan,mode,banned,banner,time]) ->
         Just (RplQuietList (mkId chan) (B8.head mode) banned banner (asTimeStamp time))

    ("729",[_,chan,mode,_]) ->
         Just (RplEndOfQuietList (mkId chan) (B8.head mode))

    ("742",[_,chan,mode,setting,_]) ->
         Just (Err (mkId chan) (ErrMlockRestricted (B8.head mode) setting))

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

    ("PONG",[server    ]) -> Just (Pong server Nothing)
    ("PONG",[server,txt]) -> Just (Pong server (Just txt))

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
