{-# Language PatternSynonyms, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-|
Module      : Irc.Codes
Description : Helpers for interpreting IRC reply codes
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines support for working with IRC's numeric reply
codes. Pattern synonyms are provided for each of the possible IRC reply codes.

Reply code information was extracted from https://www.alien.net.au/irc/irc2numerics.html

-}

module Irc.Codes where

import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Type of numeric reply codes
newtype ReplyCode = ReplyCode Word
  deriving (Eq, Ord)

instance Show ReplyCode where
  showsPrec p (ReplyCode x) = showsPrec p x

-- | Categories for reply codes
data ReplyType
  = ClientServerReply -- ^ 0-99 Messages between client and server
  | CommandReply      -- ^ 200-399 Responses to commands
  | ErrorReply        -- ^ 200-399 Errors
  | UnknownReply      -- ^ Uncategorized

pattern RPL_WELCOME                 = ReplyCode 001
pattern RPL_YOURHOST                = ReplyCode 002
pattern RPL_CREATED                 = ReplyCode 003
pattern RPL_MYINFO                  = ReplyCode 004
pattern RPL_ISUPPORT                = ReplyCode 005
pattern RPL_SNOMASK                 = ReplyCode 008
pattern RPL_STATMEMTOT              = ReplyCode 009
pattern RPL_REDIR                   = ReplyCode 010
pattern RPL_YOURCOOKIE              = ReplyCode 014
pattern RPL_MAP                     = ReplyCode 015
pattern RPL_MAPEND                  = ReplyCode 017
pattern RPL_YOURID                  = ReplyCode 042
pattern RPL_SAVENICK                = ReplyCode 043
pattern RPL_ATTEMPTINGJUNC          = ReplyCode 050
pattern RPL_ATTEMPTINGREROUTE       = ReplyCode 051
pattern RPL_TRACELINK               = ReplyCode 200
pattern RPL_TRACECONNECTING         = ReplyCode 201
pattern RPL_TRACEHANDSHAKE          = ReplyCode 202
pattern RPL_TRACEUNKNOWN            = ReplyCode 203
pattern RPL_TRACEOPERATOR           = ReplyCode 204
pattern RPL_TRACEUSER               = ReplyCode 205
pattern RPL_TRACESERVER             = ReplyCode 206
pattern RPL_TRACESERVICE            = ReplyCode 207
pattern RPL_TRACENEWTYPE            = ReplyCode 208
pattern RPL_TRACECLASS              = ReplyCode 209
pattern RPL_TRACERECONNECT          = ReplyCode 210
pattern RPL_STATS                   = ReplyCode 210
pattern RPL_STATSLINKINFO           = ReplyCode 211
pattern RPL_STATSCOMMANDS           = ReplyCode 212
pattern RPL_STATSCLINE              = ReplyCode 213
pattern RPL_STATSNLINE              = ReplyCode 214
pattern RPL_STATSILINE              = ReplyCode 215
pattern RPL_STATSKLINE              = ReplyCode 216
pattern RPL_STATSQLINE              = ReplyCode 217
pattern RPL_STATSYLINE              = ReplyCode 218
pattern RPL_ENDOFSTATS              = ReplyCode 219
pattern RPL_STATSPLINE              = ReplyCode 220
pattern RPL_UMODEIS                 = ReplyCode 221
pattern RPL_SQLINE_NICK             = ReplyCode 222
pattern RPL_STATSDLINE              = ReplyCode 225
pattern RPL_STATSZLINE              = ReplyCode 225
pattern RPL_STATSCOUNT              = ReplyCode 226
pattern RPL_SERVICEINFO             = ReplyCode 231
pattern RPL_ENDOFSERVICES           = ReplyCode 232
pattern RPL_SERVICE                 = ReplyCode 233
pattern RPL_SERVLIST                = ReplyCode 234
pattern RPL_SERVLISTEND             = ReplyCode 235
pattern RPL_STATSVERBOSE            = ReplyCode 236
pattern RPL_STATSIAUTH              = ReplyCode 239
pattern RPL_STATSLLINE              = ReplyCode 241
pattern RPL_STATSUPTIME             = ReplyCode 242
pattern RPL_STATSOLINE              = ReplyCode 243
pattern RPL_STATSHLINE              = ReplyCode 244
pattern RPL_STATSSLINE              = ReplyCode 245
pattern RPL_STATSPING               = ReplyCode 246
pattern RPL_STATSXLINE              = ReplyCode 247
pattern RPL_STATSULINE              = ReplyCode 248
pattern RPL_STATSDEBUG              = ReplyCode 249
pattern RPL_STATSCONN               = ReplyCode 250
pattern RPL_LUSERCLIENT             = ReplyCode 251
pattern RPL_LUSEROP                 = ReplyCode 252
pattern RPL_LUSERUNKNOWN            = ReplyCode 253
pattern RPL_LUSERCHANNELS           = ReplyCode 254
pattern RPL_LUSERME                 = ReplyCode 255
pattern RPL_ADMINME                 = ReplyCode 256
pattern RPL_ADMINLOC1               = ReplyCode 257
pattern RPL_ADMINLOC2               = ReplyCode 258
pattern RPL_ADMINEMAIL              = ReplyCode 259
pattern RPL_TRACELOG                = ReplyCode 261
pattern RPL_ENDOFTRACE              = ReplyCode 262
pattern RPL_LOAD2HI                 = ReplyCode 263
pattern RPL_LOCALUSERS              = ReplyCode 265
pattern RPL_GLOBALUSERS             = ReplyCode 266
pattern RPL_START_NETSTAT           = ReplyCode 267
pattern RPL_NETSTAT                 = ReplyCode 268
pattern RPL_END_NETSTAT             = ReplyCode 269
pattern RPL_PRIVS                   = ReplyCode 270
pattern RPL_SILELIST                = ReplyCode 271
pattern RPL_ENDOFSILELIST           = ReplyCode 272
pattern RPL_NOTIFY                  = ReplyCode 273
pattern RPL_ENDNOTIFY               = ReplyCode 274
pattern RPL_STATSDELTA              = ReplyCode 274
pattern RPL_WHOISCERTFP             = ReplyCode 276
pattern RPL_VCHANLIST               = ReplyCode 277
pattern RPL_VCHANHELP               = ReplyCode 278
pattern RPL_GLIST                   = ReplyCode 280
pattern RPL_ACCEPTLIST              = ReplyCode 281
pattern RPL_ENDOFACCEPT             = ReplyCode 282
pattern RPL_ENDOFJUPELIST           = ReplyCode 283
pattern RPL_FEATURE                 = ReplyCode 284
pattern RPL_DATASTR                 = ReplyCode 290
pattern RPL_END_CHANINFO            = ReplyCode 299
pattern RPL_NONE                    = ReplyCode 300
pattern RPL_AWAY                    = ReplyCode 301
pattern RPL_USERHOST                = ReplyCode 302
pattern RPL_ISON                    = ReplyCode 303
pattern RPL_TEXT                    = ReplyCode 304
pattern RPL_UNAWAY                  = ReplyCode 305
pattern RPL_NOWAWAY                 = ReplyCode 306
pattern RPL_WHOISREGNICK            = ReplyCode 307
pattern RPL_SUSERHOST               = ReplyCode 307
pattern RPL_NOTIFYACTION            = ReplyCode 308
pattern RPL_WHOISADMIN              = ReplyCode 308
pattern RPL_NICKTRACE               = ReplyCode 309
pattern RPL_WHOISSADMIN             = ReplyCode 309
pattern RPL_WHOISHELPER             = ReplyCode 309
pattern RPL_WHOISUSER               = ReplyCode 311
pattern RPL_WHOISSERVER             = ReplyCode 312
pattern RPL_WHOISOPERATOR           = ReplyCode 313
pattern RPL_WHOWASUSER              = ReplyCode 314
pattern RPL_ENDOFWHO                = ReplyCode 315
pattern RPL_WHOISCHANOP             = ReplyCode 316
pattern RPL_WHOISIDLE               = ReplyCode 317
pattern RPL_ENDOFWHOIS              = ReplyCode 318
pattern RPL_WHOISCHANNELS           = ReplyCode 319
pattern RPL_WHOISSPECIAL            = ReplyCode 320
pattern RPL_LISTSTART               = ReplyCode 321
pattern RPL_LIST                    = ReplyCode 322
pattern RPL_LISTEND                 = ReplyCode 323
pattern RPL_CHANNELMODEIS           = ReplyCode 324
pattern RPL_CHANNELMLOCKIS          = ReplyCode 325
pattern RPL_NOCHANPASS              = ReplyCode 326
pattern RPL_CHPASSUNKNOWN           = ReplyCode 327
pattern RPL_CHANNEL_URL             = ReplyCode 328
pattern RPL_CREATIONTIME            = ReplyCode 329
pattern RPL_WHOWAS_TIME             = ReplyCode 330
pattern RPL_WHOISACCOUNT            = ReplyCode 330
pattern RPL_NOTOPIC                 = ReplyCode 331
pattern RPL_TOPIC                   = ReplyCode 332
pattern RPL_TOPICWHOTIME            = ReplyCode 333
pattern RPL_LISTUSAGE               = ReplyCode 334
pattern RPL_COMMANDSYNTAX           = ReplyCode 334
pattern RPL_LISTSYNTAX              = ReplyCode 334
pattern RPL_WHOISACTUALLY           = ReplyCode 338
pattern RPL_BADCHANPASS             = ReplyCode 339
pattern RPL_INVITING                = ReplyCode 341
pattern RPL_SUMMONING               = ReplyCode 342
pattern RPL_INVITED                 = ReplyCode 345
pattern RPL_INVEXLIST               = ReplyCode 346
pattern RPL_ENDOFINVEXLIST          = ReplyCode 347
pattern RPL_EXCEPTLIST              = ReplyCode 348
pattern RPL_ENDOFEXCEPTLIST         = ReplyCode 349
pattern RPL_VERSION                 = ReplyCode 351
pattern RPL_WHOREPLY                = ReplyCode 352
pattern RPL_NAMREPLY                = ReplyCode 353
pattern RPL_WHOSPCRPL               = ReplyCode 354
pattern RPL_NAMREPLY_               = ReplyCode 355
pattern RPL_WHOWASREAL              = ReplyCode 360
pattern RPL_KILLDONE                = ReplyCode 361
pattern RPL_CLOSING                 = ReplyCode 362
pattern RPL_CLOSEEND                = ReplyCode 363
pattern RPL_LINKS                   = ReplyCode 364
pattern RPL_ENDOFLINKS              = ReplyCode 365
pattern RPL_ENDOFNAMES              = ReplyCode 366
pattern RPL_BANLIST                 = ReplyCode 367
pattern RPL_ENDOFBANLIST            = ReplyCode 368
pattern RPL_ENDOFWHOWAS             = ReplyCode 369
pattern RPL_INFO                    = ReplyCode 371
pattern RPL_MOTD                    = ReplyCode 372
pattern RPL_INFOSTART               = ReplyCode 373
pattern RPL_ENDOFINFO               = ReplyCode 374
pattern RPL_MOTDSTART               = ReplyCode 375
pattern RPL_ENDOFMOTD               = ReplyCode 376
pattern RPL_WHOISHOST               = ReplyCode 378
pattern RPL_KICKLINKED              = ReplyCode 379
pattern RPL_YOUREOPER               = ReplyCode 381
pattern RPL_REHASHING               = ReplyCode 382
pattern RPL_YOURESERVICE            = ReplyCode 383
pattern RPL_MYPORTIS                = ReplyCode 384
pattern RPL_NOTOPERANYMORE          = ReplyCode 385
pattern RPL_RSACHALLENGE            = ReplyCode 386
pattern RPL_TIME                    = ReplyCode 391
pattern RPL_USERSSTART              = ReplyCode 392
pattern RPL_USERS                   = ReplyCode 393
pattern RPL_ENDOFUSERS              = ReplyCode 394
pattern RPL_NOUSERS                 = ReplyCode 395
pattern RPL_HOSTHIDDEN              = ReplyCode 396
pattern ERR_UNKNOWNERROR            = ReplyCode 400
pattern ERR_NOSUCHNICK              = ReplyCode 401
pattern ERR_NOSUCHSERVER            = ReplyCode 402
pattern ERR_NOSUCHCHANNEL           = ReplyCode 403
pattern ERR_CANNOTSENDTOCHAN        = ReplyCode 404
pattern ERR_TOOMANYCHANNELS         = ReplyCode 405
pattern ERR_WASNOSUCHNICK           = ReplyCode 406
pattern ERR_TOOMANYTARGETS          = ReplyCode 407
pattern ERR_NOORIGIN                = ReplyCode 409
pattern ERR_NORECIPIENT             = ReplyCode 411
pattern ERR_NOTEXTTOSEND            = ReplyCode 412
pattern ERR_NOTOPLEVEL              = ReplyCode 413
pattern ERR_WILDTOPLEVEL            = ReplyCode 414
pattern ERR_BADMASK                 = ReplyCode 415
pattern ERR_TOOMANYMATCHES          = ReplyCode 416
pattern ERR_LENGTHTRUNCATED         = ReplyCode 419
pattern ERR_UNKNOWNCOMMAND          = ReplyCode 421
pattern ERR_NOMOTD                  = ReplyCode 422
pattern ERR_NOADMININFO             = ReplyCode 423
pattern ERR_FILEERROR               = ReplyCode 424
pattern ERR_NOOPERMOTD              = ReplyCode 425
pattern ERR_TOOMANYAWAY             = ReplyCode 429
pattern ERR_EVENTNICKCHANGE         = ReplyCode 430
pattern ERR_NONICKNAMEGIVEN         = ReplyCode 431
pattern ERR_ERRONEUSNICKNAME        = ReplyCode 432
pattern ERR_NICKNAMEINUSE           = ReplyCode 433
pattern ERR_SERVICENAMEINUSE        = ReplyCode 434
pattern ERR_NORULES                 = ReplyCode 434
pattern ERR_BANNICKCHANGE           = ReplyCode 435
pattern ERR_NICKCOLLISION           = ReplyCode 436
pattern ERR_UNAVAILRESOURCE         = ReplyCode 437
pattern ERR_NICKTOOFAST             = ReplyCode 438
pattern ERR_TARGETTOOFAST           = ReplyCode 439
pattern ERR_SERVICESDOWN            = ReplyCode 440
pattern ERR_USERNOTINCHANNEL        = ReplyCode 441
pattern ERR_NOTONCHANNEL            = ReplyCode 442
pattern ERR_USERONCHANNEL           = ReplyCode 443
pattern ERR_NOLOGIN                 = ReplyCode 444
pattern ERR_SUMMONDISABLED          = ReplyCode 445
pattern ERR_USERSDISABLED           = ReplyCode 446
pattern ERR_NONICKCHANGE            = ReplyCode 447
pattern ERR_NOTIMPLEMENTED          = ReplyCode 449
pattern ERR_NOTREGISTERED           = ReplyCode 451
pattern ERR_IDCOLLISION             = ReplyCode 452
pattern ERR_NICKLOST                = ReplyCode 453
pattern ERR_HOSTILENAME             = ReplyCode 455
pattern ERR_ACCEPTFULL              = ReplyCode 456
pattern ERR_ACCEPTEXIST             = ReplyCode 457
pattern ERR_ACCEPTNOT               = ReplyCode 458
pattern ERR_NOHIDING                = ReplyCode 459
pattern ERR_NOTFORHALFOPS           = ReplyCode 460
pattern ERR_NEEDMOREPARAMS          = ReplyCode 461
pattern ERR_ALREADYREGISTERED       = ReplyCode 462
pattern ERR_NOPERMFORHOST           = ReplyCode 463
pattern ERR_PASSWDMISMATCH          = ReplyCode 464
pattern ERR_YOUREBANNEDCREEP        = ReplyCode 465
pattern ERR_YOUWILLBEBANNED         = ReplyCode 466
pattern ERR_KEYSET                  = ReplyCode 467
pattern ERR_INVALIDUSERNAME         = ReplyCode 468
pattern ERR_ONLYSERVERSCANCHANGE    = ReplyCode 468
pattern ERR_LINKSET                 = ReplyCode 469
pattern ERR_LINKCHANNEL             = ReplyCode 470
pattern ERR_CHANNELISFULL           = ReplyCode 471
pattern ERR_UNKNOWNMODE             = ReplyCode 472
pattern ERR_INVITEONLYCHAN          = ReplyCode 473
pattern ERR_BANNEDFROMCHAN          = ReplyCode 474
pattern ERR_BADCHANNELKEY           = ReplyCode 475
pattern ERR_BADCHANMASK             = ReplyCode 476
pattern ERR_NEEDREGGEDNICK          = ReplyCode 477
pattern ERR_BANLISTFULL             = ReplyCode 478
pattern ERR_BADCHANNAME             = ReplyCode 479
pattern ERR_THROTTLE                = ReplyCode 480
pattern ERR_NOPRIVILEGES            = ReplyCode 481
pattern ERR_CHANOPRIVSNEEDED        = ReplyCode 482
pattern ERR_CANTKILLSERVER          = ReplyCode 483
pattern ERR_ISCHANSERVICE           = ReplyCode 484
pattern ERR_BANNEDNICK              = ReplyCode 485
pattern ERR_NONONREG                = ReplyCode 486
pattern ERR_TSLESSCHAN              = ReplyCode 488
pattern ERR_VOICENEEDED             = ReplyCode 489
pattern ERR_NOOPERHOST              = ReplyCode 491
pattern ERR_NOSERVICEHOST           = ReplyCode 492
pattern ERR_NOFEATURE               = ReplyCode 493
pattern ERR_OWNMODE                 = ReplyCode 494
pattern ERR_BADLOGTYPE              = ReplyCode 495
pattern ERR_BADLOGSYS               = ReplyCode 496
pattern ERR_BADLOGVALUE             = ReplyCode 497
pattern ERR_ISOPERLCHAN             = ReplyCode 498
pattern ERR_CHANOWNPRIVNEEDED       = ReplyCode 499
pattern ERR_UMODEUNKNOWNFLAG        = ReplyCode 501
pattern ERR_USERSDONTMATCH          = ReplyCode 502
pattern ERR_GHOSTEDCLIENT           = ReplyCode 503
pattern ERR_USERNOTONSERV           = ReplyCode 504
pattern ERR_SILELISTFULL            = ReplyCode 511
pattern ERR_TOOMANYWATCH            = ReplyCode 512
pattern ERR_WRONGPONG               = ReplyCode 513
pattern ERR_BADEXPIRE               = ReplyCode 515
pattern ERR_DONTCHEAT               = ReplyCode 516
pattern ERR_DISABLED                = ReplyCode 517
pattern ERR_NOINVITE                = ReplyCode 518
pattern ERR_LONGMASK                = ReplyCode 518
pattern ERR_ADMONLY                 = ReplyCode 519
pattern ERR_TOOMANYUSERS            = ReplyCode 519
pattern ERR_OPERONLY                = ReplyCode 520
pattern ERR_MASKTOOWIDE             = ReplyCode 520
pattern ERR_WHOTRUNC                = ReplyCode 520
pattern ERR_LISTSYNTAX              = ReplyCode 521
pattern ERR_WHOSYNTAX               = ReplyCode 522
pattern ERR_WHOLIMEXCEED            = ReplyCode 523
pattern ERR_HELPNOTFOUND            = ReplyCode 524
pattern ERR_REMOTEPFX               = ReplyCode 525
pattern ERR_PFXUNROUTABLE           = ReplyCode 526
pattern ERR_BADHOSTMASK             = ReplyCode 550
pattern ERR_HOSTUNAVAIL             = ReplyCode 551
pattern ERR_USINGSLINE              = ReplyCode 552
pattern ERR_STATSSLINE              = ReplyCode 553
pattern RPL_LOGON                   = ReplyCode 600
pattern RPL_LOGOFF                  = ReplyCode 601
pattern RPL_WATCHOFF                = ReplyCode 602
pattern RPL_WATCHSTAT               = ReplyCode 603
pattern RPL_NOWON                   = ReplyCode 604
pattern RPL_NOWOFF                  = ReplyCode 605
pattern RPL_WATCHLIST               = ReplyCode 606
pattern RPL_ENDOFWATCHLIST          = ReplyCode 607
pattern RPL_WATCHCLEAR              = ReplyCode 608
pattern RPL_ISOPER                  = ReplyCode 610
pattern RPL_ISLOCOP                 = ReplyCode 611
pattern RPL_ISNOTOPER               = ReplyCode 612
pattern RPL_ENDOFISOPER             = ReplyCode 613
pattern RPL_DCCSTATUS               = ReplyCode 617
pattern RPL_DCCLIST                 = ReplyCode 618
pattern RPL_ENDOFDCCLIST            = ReplyCode 619
pattern RPL_WHOWASHOST              = ReplyCode 619
pattern RPL_DCCINFO                 = ReplyCode 620
pattern RPL_RULES                   = ReplyCode 621
pattern RPL_ENDOFO                  = ReplyCode 626
pattern RPL_SETTINGS                = ReplyCode 630
pattern RPL_ENDOFSETTINGS           = ReplyCode 631
pattern RPL_DUMPING                 = ReplyCode 640
pattern RPL_DUMPRPL                 = ReplyCode 641
pattern RPL_EODUMP                  = ReplyCode 642
pattern RPL_TRACEROUTE_HOP          = ReplyCode 660
pattern RPL_TRACEROUTE_START        = ReplyCode 661
pattern RPL_MODECHANGEWARN          = ReplyCode 662
pattern RPL_CHANREDIR               = ReplyCode 663
pattern RPL_SERVMODEIS              = ReplyCode 664
pattern RPL_OTHERUMODEIS            = ReplyCode 665
pattern RPL_ENDOF_GENERIC           = ReplyCode 666
pattern RPL_WHOWASDETAILS           = ReplyCode 670
pattern RPL_WHOISSECURE             = ReplyCode 671
pattern RPL_UNKNOWNMODES            = ReplyCode 672
pattern RPL_CANNOTSETMODES          = ReplyCode 673
pattern RPL_LUSERSTAFF              = ReplyCode 678
pattern RPL_TIMEONSERVERIS          = ReplyCode 679
pattern RPL_NETWORKS                = ReplyCode 682
pattern RPL_YOURLANGUAGEIS          = ReplyCode 687
pattern RPL_LANGUAGE                = ReplyCode 688
pattern RPL_WHOISSTAFF              = ReplyCode 689
pattern RPL_WHOISLANGUAGE           = ReplyCode 690
pattern RPL_MODLIST                 = ReplyCode 702
pattern RPL_ENDOFMODLIST            = ReplyCode 703
pattern RPL_HELPSTART               = ReplyCode 704
pattern RPL_HELPTXT                 = ReplyCode 705
pattern RPL_ENDOFHELP               = ReplyCode 706
pattern ERR_TARGCHANGE              = ReplyCode 707
pattern RPL_ETRACEFULL              = ReplyCode 708
pattern RPL_ETRACE                  = ReplyCode 709
pattern RPL_KNOCK                   = ReplyCode 710
pattern RPL_KNOCKDLVR               = ReplyCode 711
pattern ERR_TOOMANYKNOCK            = ReplyCode 712
pattern ERR_CHANOPEN                = ReplyCode 713
pattern ERR_KNOCKONCHAN             = ReplyCode 714
pattern ERR_KNOCKDISABLED           = ReplyCode 715
pattern RPL_TARGUMODEG              = ReplyCode 716
pattern RPL_TARGNOTIFY              = ReplyCode 717
pattern RPL_UMODEGMSG               = ReplyCode 718
pattern RPL_OMOTDSTART              = ReplyCode 720
pattern RPL_OMOTD                   = ReplyCode 721
pattern RPL_ENDOFOMOTD              = ReplyCode 722
pattern ERR_NOPRIVS                 = ReplyCode 723
pattern RPL_TESTMASK                = ReplyCode 724
pattern RPL_TESTLINE                = ReplyCode 725
pattern RPL_NOTESTLINE              = ReplyCode 726
pattern RPL_QUIETLIST               = ReplyCode 728
pattern RPL_ENDOFQUIETLIST          = ReplyCode 729
pattern RPL_MONONLINE               = ReplyCode 730
pattern RPL_MONOFFLINE              = ReplyCode 731
pattern RPL_MONLIST                 = ReplyCode 732
pattern RPL_ENDOFMONLIST            = ReplyCode 733
pattern ERR_MONLISTFULL             = ReplyCode 734
pattern RPL_RSACHALLENGE2           = ReplyCode 740
pattern RPL_ENDOFRSACHALLENGE2      = ReplyCode 741
pattern ERR_MLOCKRESTRICTED         = ReplyCode 742
pattern RPL_SCANMATCHED             = ReplyCode 750
pattern RPL_SCANUMODES              = ReplyCode 751
pattern RPL_XINFO                   = ReplyCode 771
pattern RPL_XINFOSTART              = ReplyCode 773
pattern RPL_XINFOEND                = ReplyCode 774
pattern RPL_LOGGEDIN                = ReplyCode 900
pattern RPL_LOGGEDOUT               = ReplyCode 901
pattern RPL_NICKLOCKED              = ReplyCode 902
pattern RPL_SASLSUCCESS             = ReplyCode 903
pattern RPL_SASLFAIL                = ReplyCode 904
pattern RPL_SASLTOOLONG             = ReplyCode 905
pattern RPL_SASLABORTED             = ReplyCode 906
pattern RPL_SASLALREADY             = ReplyCode 907
pattern RPL_SASLMECHS               = ReplyCode 908
pattern ERR_CANNOTDOCOMMAND         = ReplyCode 972
pattern ERR_CANNOTCHANGEUMODE       = ReplyCode 973
pattern ERR_CANNOTCHANGECHANMODE    = ReplyCode 974
pattern ERR_CANNOTCHANGESERVERMODE  = ReplyCode 975
pattern ERR_CANNOTSENDTONICK        = ReplyCode 976
pattern ERR_UNKNOWNSERVERMODE       = ReplyCode 977
pattern ERR_SERVERMODELOCK          = ReplyCode 979
pattern ERR_BADCHARENCODING         = ReplyCode 980
pattern ERR_TOOMANYLANGUAGES        = ReplyCode 981
pattern ERR_NOLANGUAGE              = ReplyCode 982
pattern ERR_TEXTTOOSHORT            = ReplyCode 983
pattern ERR_NUMERIC_ERR             = ReplyCode 999

data ReplyCodeInfo = ReplyCodeInfo
  { replyCodeType :: !ReplyType
  , replyCodeText :: !Text
  }

replyCodeInfo :: ReplyCode -> ReplyCodeInfo
replyCodeInfo (ReplyCode w) =
  case replyCodeInfoTable Vector.!? i of
    Nothing -> defaultReplyCodeInfo i
    Just info -> info
  where
    i = fromIntegral w

defaultReplyCodeInfo :: Int -> ReplyCodeInfo
defaultReplyCodeInfo = ReplyCodeInfo UnknownReply . Text.pack . show

replyCodeInfoTable :: Vector ReplyCodeInfo
replyCodeInfoTable
  = Vector.accumulate
      (\_def new -> new)
      (Vector.generate 1000 defaultReplyCodeInfo)
  $ fmap (\(ReplyCode code,info) -> (fromIntegral code, info))
  $ Vector.fromList
  [ (RPL_WELCOME               , ReplyCodeInfo ClientServerReply "WELCOME")
  , (RPL_YOURHOST              , ReplyCodeInfo ClientServerReply "YOURHOST")
  , (RPL_CREATED               , ReplyCodeInfo ClientServerReply "CREATED")
  , (RPL_MYINFO                , ReplyCodeInfo ClientServerReply "MYINFO")
  , (RPL_ISUPPORT              , ReplyCodeInfo ClientServerReply "ISUPPORT")
  , (RPL_SNOMASK               , ReplyCodeInfo ClientServerReply "SNOMASK")
  , (RPL_STATMEMTOT            , ReplyCodeInfo ClientServerReply "STATMEMTOT")
  , (RPL_REDIR                 , ReplyCodeInfo ClientServerReply "REDIR")
  , (RPL_YOURCOOKIE            , ReplyCodeInfo ClientServerReply "YOURCOOKIE")
  , (RPL_MAP                   , ReplyCodeInfo ClientServerReply "MAP")
  , (RPL_MAPEND                , ReplyCodeInfo ClientServerReply "MAPEND")
  , (RPL_YOURID                , ReplyCodeInfo ClientServerReply "YOURID")
  , (RPL_SAVENICK              , ReplyCodeInfo ClientServerReply "SAVENICK")
  , (RPL_ATTEMPTINGJUNC        , ReplyCodeInfo ClientServerReply "ATTEMPTINGJUNC")
  , (RPL_ATTEMPTINGREROUTE     , ReplyCodeInfo ClientServerReply "ATTEMPTINGREROUTE")
  , (RPL_TRACELINK             , ReplyCodeInfo CommandReply "TRACELINK")
  , (RPL_TRACECONNECTING       , ReplyCodeInfo CommandReply "TRACECONNECTING")
  , (RPL_TRACEHANDSHAKE        , ReplyCodeInfo CommandReply "TRACEHANDSHAKE")
  , (RPL_TRACEUNKNOWN          , ReplyCodeInfo CommandReply "TRACEUNKNOWN")
  , (RPL_TRACEOPERATOR         , ReplyCodeInfo CommandReply "TRACEOPERATOR")
  , (RPL_TRACEUSER             , ReplyCodeInfo CommandReply "TRACEUSER")
  , (RPL_TRACESERVER           , ReplyCodeInfo CommandReply "TRACESERVER")
  , (RPL_TRACESERVICE          , ReplyCodeInfo CommandReply "TRACESERVICE")
  , (RPL_TRACENEWTYPE          , ReplyCodeInfo CommandReply "TRACENEWTYPE")
  , (RPL_TRACECLASS            , ReplyCodeInfo CommandReply "TRACECLASS")
  , (RPL_TRACERECONNECT        , ReplyCodeInfo CommandReply "TRACERECONNECT")
  , (RPL_STATS                 , ReplyCodeInfo CommandReply "STATS")
  , (RPL_STATSLINKINFO         , ReplyCodeInfo CommandReply "STATSLINKINFO")
  , (RPL_STATSCOMMANDS         , ReplyCodeInfo CommandReply "STATSCOMMANDS")
  , (RPL_STATSCLINE            , ReplyCodeInfo CommandReply "STATSCLINE")
  , (RPL_STATSNLINE            , ReplyCodeInfo CommandReply "STATSNLINE")
  , (RPL_STATSILINE            , ReplyCodeInfo CommandReply "STATSILINE")
  , (RPL_STATSKLINE            , ReplyCodeInfo CommandReply "STATSKLINE")
  , (RPL_STATSQLINE            , ReplyCodeInfo CommandReply "STATSQLINE")
  , (RPL_STATSYLINE            , ReplyCodeInfo CommandReply "STATSYLINE")
  , (RPL_ENDOFSTATS            , ReplyCodeInfo CommandReply "ENDOFSTATS")
  , (RPL_STATSPLINE            , ReplyCodeInfo CommandReply "STATSPLINE")
  , (RPL_UMODEIS               , ReplyCodeInfo CommandReply "UMODEIS")
  , (RPL_SQLINE_NICK           , ReplyCodeInfo CommandReply "SQLINE_NICK")
  , (RPL_STATSDLINE            , ReplyCodeInfo CommandReply "STATSDLINE")
  , (RPL_STATSZLINE            , ReplyCodeInfo CommandReply "STATSZLINE")
  , (RPL_STATSCOUNT            , ReplyCodeInfo CommandReply "STATSCOUNT")
  , (RPL_SERVICEINFO           , ReplyCodeInfo CommandReply "SERVICEINFO")
  , (RPL_ENDOFSERVICES         , ReplyCodeInfo CommandReply "ENDOFSERVICES")
  , (RPL_SERVICE               , ReplyCodeInfo CommandReply "SERVICE")
  , (RPL_SERVLIST              , ReplyCodeInfo CommandReply "SERVLIST")
  , (RPL_SERVLISTEND           , ReplyCodeInfo CommandReply "SERVLISTEND")
  , (RPL_STATSVERBOSE          , ReplyCodeInfo CommandReply "STATSVERBOSE")
  , (RPL_STATSIAUTH            , ReplyCodeInfo CommandReply "STATSIAUTH")
  , (RPL_STATSLLINE            , ReplyCodeInfo CommandReply "STATSLLINE")
  , (RPL_STATSUPTIME           , ReplyCodeInfo CommandReply "STATSUPTIME")
  , (RPL_STATSOLINE            , ReplyCodeInfo CommandReply "STATSOLINE")
  , (RPL_STATSHLINE            , ReplyCodeInfo CommandReply "STATSHLINE")
  , (RPL_STATSSLINE            , ReplyCodeInfo CommandReply "STATSSLINE")
  , (RPL_STATSPING             , ReplyCodeInfo CommandReply "STATSPING")
  , (RPL_STATSXLINE            , ReplyCodeInfo CommandReply "STATSXLINE")
  , (RPL_STATSULINE            , ReplyCodeInfo CommandReply "STATSULINE")
  , (RPL_STATSDEBUG            , ReplyCodeInfo CommandReply "STATSDEBUG")
  , (RPL_STATSCONN             , ReplyCodeInfo CommandReply "STATSCONN")
  , (RPL_LUSERCLIENT           , ReplyCodeInfo CommandReply "LUSERCLIENT")
  , (RPL_LUSEROP               , ReplyCodeInfo CommandReply "LUSEROP")
  , (RPL_LUSERUNKNOWN          , ReplyCodeInfo CommandReply "LUSERUNKNOWN")
  , (RPL_LUSERCHANNELS         , ReplyCodeInfo CommandReply "LUSERCHANNELS")
  , (RPL_LUSERME               , ReplyCodeInfo CommandReply "LUSERME")
  , (RPL_ADMINME               , ReplyCodeInfo CommandReply "ADMINME")
  , (RPL_ADMINLOC1             , ReplyCodeInfo CommandReply "ADMINLOC1")
  , (RPL_ADMINLOC2             , ReplyCodeInfo CommandReply "ADMINLOC2")
  , (RPL_ADMINEMAIL            , ReplyCodeInfo CommandReply "ADMINEMAIL")
  , (RPL_TRACELOG              , ReplyCodeInfo CommandReply "TRACELOG")
  , (RPL_ENDOFTRACE            , ReplyCodeInfo CommandReply "ENDOFTRACE")
  , (RPL_LOAD2HI               , ReplyCodeInfo CommandReply "LOAD2HI")
  , (RPL_LOCALUSERS            , ReplyCodeInfo CommandReply "LOCALUSERS")
  , (RPL_GLOBALUSERS           , ReplyCodeInfo CommandReply "GLOBALUSERS")
  , (RPL_START_NETSTAT         , ReplyCodeInfo CommandReply "START_NETSTAT")
  , (RPL_NETSTAT               , ReplyCodeInfo CommandReply "NETSTAT")
  , (RPL_END_NETSTAT           , ReplyCodeInfo CommandReply "END_NETSTAT")
  , (RPL_PRIVS                 , ReplyCodeInfo CommandReply "PRIVS")
  , (RPL_SILELIST              , ReplyCodeInfo CommandReply "SILELIST")
  , (RPL_ENDOFSILELIST         , ReplyCodeInfo CommandReply "ENDOFSILELIST")
  , (RPL_NOTIFY                , ReplyCodeInfo CommandReply "NOTIFY")
  , (RPL_ENDNOTIFY             , ReplyCodeInfo CommandReply "ENDNOTIFY")
  , (RPL_STATSDELTA            , ReplyCodeInfo CommandReply "STATSDELTA")
  , (RPL_WHOISCERTFP           , ReplyCodeInfo CommandReply "WHOISCERTFP")
  , (RPL_VCHANLIST             , ReplyCodeInfo CommandReply "VCHANLIST")
  , (RPL_VCHANHELP             , ReplyCodeInfo CommandReply "VCHANHELP")
  , (RPL_GLIST                 , ReplyCodeInfo CommandReply "GLIST")
  , (RPL_ACCEPTLIST            , ReplyCodeInfo CommandReply "ACCEPTLIST")
  , (RPL_ENDOFACCEPT           , ReplyCodeInfo CommandReply "ENDOFACCEPT")
  , (RPL_ENDOFJUPELIST         , ReplyCodeInfo CommandReply "ENDOFJUPELIST")
  , (RPL_FEATURE               , ReplyCodeInfo CommandReply "FEATURE")
  , (RPL_DATASTR               , ReplyCodeInfo CommandReply "DATASTR")
  , (RPL_END_CHANINFO          , ReplyCodeInfo CommandReply "END_CHANINFO")
  , (RPL_NONE                  , ReplyCodeInfo CommandReply "NONE")
  , (RPL_AWAY                  , ReplyCodeInfo CommandReply "AWAY")
  , (RPL_USERHOST              , ReplyCodeInfo CommandReply "USERHOST")
  , (RPL_ISON                  , ReplyCodeInfo CommandReply "ISON")
  , (RPL_TEXT                  , ReplyCodeInfo CommandReply "TEXT")
  , (RPL_UNAWAY                , ReplyCodeInfo CommandReply "UNAWAY")
  , (RPL_NOWAWAY               , ReplyCodeInfo CommandReply "NOWAWAY")
  , (RPL_WHOISREGNICK          , ReplyCodeInfo CommandReply "WHOISREGNICK")
  , (RPL_SUSERHOST             , ReplyCodeInfo CommandReply "SUSERHOST")
  , (RPL_NOTIFYACTION          , ReplyCodeInfo CommandReply "NOTIFYACTION")
  , (RPL_WHOISADMIN            , ReplyCodeInfo CommandReply "WHOISADMIN")
  , (RPL_NICKTRACE             , ReplyCodeInfo CommandReply "NICKTRACE")
  , (RPL_WHOISSADMIN           , ReplyCodeInfo CommandReply "WHOISSADMIN")
  , (RPL_WHOISHELPER           , ReplyCodeInfo CommandReply "WHOISHELPER")
  , (RPL_WHOISUSER             , ReplyCodeInfo CommandReply "WHOISUSER")
  , (RPL_WHOISSERVER           , ReplyCodeInfo CommandReply "WHOISSERVER")
  , (RPL_WHOISOPERATOR         , ReplyCodeInfo CommandReply "WHOISOPERATOR")
  , (RPL_WHOWASUSER            , ReplyCodeInfo CommandReply "WHOWASUSER")
  , (RPL_ENDOFWHO              , ReplyCodeInfo CommandReply "ENDOFWHO")
  , (RPL_WHOISCHANOP           , ReplyCodeInfo CommandReply "WHOISCHANOP")
  , (RPL_WHOISIDLE             , ReplyCodeInfo CommandReply "WHOISIDLE")
  , (RPL_ENDOFWHOIS            , ReplyCodeInfo CommandReply "ENDOFWHOIS")
  , (RPL_WHOISCHANNELS         , ReplyCodeInfo CommandReply "WHOISCHANNELS")
  , (RPL_WHOISSPECIAL          , ReplyCodeInfo CommandReply "WHOISSPECIAL")
  , (RPL_LISTSTART             , ReplyCodeInfo CommandReply "LISTSTART")
  , (RPL_LIST                  , ReplyCodeInfo CommandReply "LIST")
  , (RPL_LISTEND               , ReplyCodeInfo CommandReply "LISTEND")
  , (RPL_CHANNELMODEIS         , ReplyCodeInfo CommandReply "CHANNELMODEIS")
  , (RPL_CHANNELMLOCKIS        , ReplyCodeInfo CommandReply "CHANNELMLOCKIS")
  , (RPL_NOCHANPASS            , ReplyCodeInfo CommandReply "NOCHANPASS")
  , (RPL_CHPASSUNKNOWN         , ReplyCodeInfo CommandReply "CHPASSUNKNOWN")
  , (RPL_CHANNEL_URL           , ReplyCodeInfo CommandReply "CHANNEL_URL")
  , (RPL_CREATIONTIME          , ReplyCodeInfo CommandReply "CREATIONTIME")
  , (RPL_WHOWAS_TIME           , ReplyCodeInfo CommandReply "WHOWAS_TIME")
  , (RPL_WHOISACCOUNT          , ReplyCodeInfo CommandReply "WHOISACCOUNT")
  , (RPL_NOTOPIC               , ReplyCodeInfo CommandReply "NOTOPIC")
  , (RPL_TOPIC                 , ReplyCodeInfo CommandReply "TOPIC")
  , (RPL_TOPICWHOTIME          , ReplyCodeInfo CommandReply "TOPICWHOTIME")
  , (RPL_LISTUSAGE             , ReplyCodeInfo CommandReply "LISTUSAGE")
  , (RPL_COMMANDSYNTAX         , ReplyCodeInfo CommandReply "COMMANDSYNTAX")
  , (RPL_LISTSYNTAX            , ReplyCodeInfo CommandReply "LISTSYNTAX")
  , (RPL_WHOISACTUALLY         , ReplyCodeInfo CommandReply "WHOISACTUALLY")
  , (RPL_BADCHANPASS           , ReplyCodeInfo CommandReply "BADCHANPASS")
  , (RPL_INVITING              , ReplyCodeInfo CommandReply "INVITING")
  , (RPL_SUMMONING             , ReplyCodeInfo CommandReply "SUMMONING")
  , (RPL_INVITED               , ReplyCodeInfo CommandReply "INVITED")
  , (RPL_INVEXLIST             , ReplyCodeInfo CommandReply "INVEXLIST")
  , (RPL_ENDOFINVEXLIST        , ReplyCodeInfo CommandReply "ENDOFINVEXLIST")
  , (RPL_EXCEPTLIST            , ReplyCodeInfo CommandReply "EXCEPTLIST")
  , (RPL_ENDOFEXCEPTLIST       , ReplyCodeInfo CommandReply "ENDOFEXCEPTLIST")
  , (RPL_VERSION               , ReplyCodeInfo CommandReply "VERSION")
  , (RPL_WHOREPLY              , ReplyCodeInfo CommandReply "WHOREPLY")
  , (RPL_NAMREPLY              , ReplyCodeInfo CommandReply "NAMREPLY")
  , (RPL_WHOSPCRPL             , ReplyCodeInfo CommandReply "WHOSPCRPL")
  , (RPL_NAMREPLY_             , ReplyCodeInfo CommandReply "NAMREPLY_")
  , (RPL_WHOWASREAL            , ReplyCodeInfo CommandReply "WHOWASREAL")
  , (RPL_KILLDONE              , ReplyCodeInfo CommandReply "KILLDONE")
  , (RPL_CLOSING               , ReplyCodeInfo CommandReply "CLOSING")
  , (RPL_CLOSEEND              , ReplyCodeInfo CommandReply "CLOSEEND")
  , (RPL_LINKS                 , ReplyCodeInfo CommandReply "LINKS")
  , (RPL_ENDOFLINKS            , ReplyCodeInfo CommandReply "ENDOFLINKS")
  , (RPL_ENDOFNAMES            , ReplyCodeInfo CommandReply "ENDOFNAMES")
  , (RPL_BANLIST               , ReplyCodeInfo CommandReply "BANLIST")
  , (RPL_ENDOFBANLIST          , ReplyCodeInfo CommandReply "ENDOFBANLIST")
  , (RPL_ENDOFWHOWAS           , ReplyCodeInfo CommandReply "ENDOFWHOWAS")
  , (RPL_INFO                  , ReplyCodeInfo CommandReply "INFO")
  , (RPL_MOTD                  , ReplyCodeInfo CommandReply "MOTD")
  , (RPL_INFOSTART             , ReplyCodeInfo CommandReply "INFOSTART")
  , (RPL_ENDOFINFO             , ReplyCodeInfo CommandReply "ENDOFINFO")
  , (RPL_MOTDSTART             , ReplyCodeInfo CommandReply "MOTDSTART")
  , (RPL_ENDOFMOTD             , ReplyCodeInfo CommandReply "ENDOFMOTD")
  , (RPL_WHOISHOST             , ReplyCodeInfo CommandReply "WHOISHOST")
  , (RPL_KICKLINKED            , ReplyCodeInfo CommandReply "KICKLINKED")
  , (RPL_YOUREOPER             , ReplyCodeInfo CommandReply "YOUREOPER")
  , (RPL_REHASHING             , ReplyCodeInfo CommandReply "REHASHING")
  , (RPL_YOURESERVICE          , ReplyCodeInfo CommandReply "YOURESERVICE")
  , (RPL_MYPORTIS              , ReplyCodeInfo CommandReply "MYPORTIS")
  , (RPL_NOTOPERANYMORE        , ReplyCodeInfo CommandReply "NOTOPERANYMORE")
  , (RPL_RSACHALLENGE          , ReplyCodeInfo CommandReply "RSACHALLENGE")
  , (RPL_TIME                  , ReplyCodeInfo CommandReply "TIME")
  , (RPL_USERSSTART            , ReplyCodeInfo CommandReply "USERSSTART")
  , (RPL_USERS                 , ReplyCodeInfo CommandReply "USERS")
  , (RPL_ENDOFUSERS            , ReplyCodeInfo CommandReply "ENDOFUSERS")
  , (RPL_NOUSERS               , ReplyCodeInfo CommandReply "NOUSERS")
  , (RPL_HOSTHIDDEN            , ReplyCodeInfo CommandReply "HOSTHIDDEN")
  , (ERR_UNKNOWNERROR          , ReplyCodeInfo ErrorReply "UNKNOWNERROR")
  , (ERR_NOSUCHNICK            , ReplyCodeInfo ErrorReply "NOSUCHNICK")
  , (ERR_NOSUCHSERVER          , ReplyCodeInfo ErrorReply "NOSUCHSERVER")
  , (ERR_NOSUCHCHANNEL         , ReplyCodeInfo ErrorReply "NOSUCHCHANNEL")
  , (ERR_CANNOTSENDTOCHAN      , ReplyCodeInfo ErrorReply "CANNOTSENDTOCHAN")
  , (ERR_TOOMANYCHANNELS       , ReplyCodeInfo ErrorReply "TOOMANYCHANNELS")
  , (ERR_WASNOSUCHNICK         , ReplyCodeInfo ErrorReply "WASNOSUCHNICK")
  , (ERR_TOOMANYTARGETS        , ReplyCodeInfo ErrorReply "TOOMANYTARGETS")
  , (ERR_NOORIGIN              , ReplyCodeInfo ErrorReply "NOORIGIN")
  , (ERR_NORECIPIENT           , ReplyCodeInfo ErrorReply "NORECIPIENT")
  , (ERR_NOTEXTTOSEND          , ReplyCodeInfo ErrorReply "NOTEXTTOSEND")
  , (ERR_NOTOPLEVEL            , ReplyCodeInfo ErrorReply "NOTOPLEVEL")
  , (ERR_WILDTOPLEVEL          , ReplyCodeInfo ErrorReply "WILDTOPLEVEL")
  , (ERR_BADMASK               , ReplyCodeInfo ErrorReply "BADMASK")
  , (ERR_TOOMANYMATCHES        , ReplyCodeInfo ErrorReply "TOOMANYMATCHES")
  , (ERR_LENGTHTRUNCATED       , ReplyCodeInfo ErrorReply "LENGTHTRUNCATED")
  , (ERR_UNKNOWNCOMMAND        , ReplyCodeInfo ErrorReply "UNKNOWNCOMMAND")
  , (ERR_NOMOTD                , ReplyCodeInfo ErrorReply "NOMOTD")
  , (ERR_NOADMININFO           , ReplyCodeInfo ErrorReply "NOADMININFO")
  , (ERR_FILEERROR             , ReplyCodeInfo ErrorReply "FILEERROR")
  , (ERR_NOOPERMOTD            , ReplyCodeInfo ErrorReply "NOOPERMOTD")
  , (ERR_TOOMANYAWAY           , ReplyCodeInfo ErrorReply "TOOMANYAWAY")
  , (ERR_EVENTNICKCHANGE       , ReplyCodeInfo ErrorReply "EVENTNICKCHANGE")
  , (ERR_NONICKNAMEGIVEN       , ReplyCodeInfo ErrorReply "NONICKNAMEGIVEN")
  , (ERR_ERRONEUSNICKNAME      , ReplyCodeInfo ErrorReply "ERRONEUSNICKNAME")
  , (ERR_NICKNAMEINUSE         , ReplyCodeInfo ErrorReply "NICKNAMEINUSE")
  , (ERR_SERVICENAMEINUSE      , ReplyCodeInfo ErrorReply "SERVICENAMEINUSE")
  , (ERR_NORULES               , ReplyCodeInfo ErrorReply "NORULES")
  , (ERR_BANNICKCHANGE         , ReplyCodeInfo ErrorReply "BANNICKCHANGE")
  , (ERR_NICKCOLLISION         , ReplyCodeInfo ErrorReply "NICKCOLLISION")
  , (ERR_UNAVAILRESOURCE       , ReplyCodeInfo ErrorReply "UNAVAILRESOURCE")
  , (ERR_NICKTOOFAST           , ReplyCodeInfo ErrorReply "NICKTOOFAST")
  , (ERR_TARGETTOOFAST         , ReplyCodeInfo ErrorReply "TARGETTOOFAST")
  , (ERR_SERVICESDOWN          , ReplyCodeInfo ErrorReply "SERVICESDOWN")
  , (ERR_USERNOTINCHANNEL      , ReplyCodeInfo ErrorReply "USERNOTINCHANNEL")
  , (ERR_NOTONCHANNEL          , ReplyCodeInfo ErrorReply "NOTONCHANNEL")
  , (ERR_USERONCHANNEL         , ReplyCodeInfo ErrorReply "USERONCHANNEL")
  , (ERR_NOLOGIN               , ReplyCodeInfo ErrorReply "NOLOGIN")
  , (ERR_SUMMONDISABLED        , ReplyCodeInfo ErrorReply "SUMMONDISABLED")
  , (ERR_USERSDISABLED         , ReplyCodeInfo ErrorReply "USERSDISABLED")
  , (ERR_NONICKCHANGE          , ReplyCodeInfo ErrorReply "NONICKCHANGE")
  , (ERR_NOTIMPLEMENTED        , ReplyCodeInfo ErrorReply "NOTIMPLEMENTED")
  , (ERR_NOTREGISTERED         , ReplyCodeInfo ErrorReply "NOTREGISTERED")
  , (ERR_IDCOLLISION           , ReplyCodeInfo ErrorReply "IDCOLLISION")
  , (ERR_NICKLOST              , ReplyCodeInfo ErrorReply "NICKLOST")
  , (ERR_HOSTILENAME           , ReplyCodeInfo ErrorReply "HOSTILENAME")
  , (ERR_ACCEPTFULL            , ReplyCodeInfo ErrorReply "ACCEPTFULL")
  , (ERR_ACCEPTEXIST           , ReplyCodeInfo ErrorReply "ACCEPTEXIST")
  , (ERR_ACCEPTNOT             , ReplyCodeInfo ErrorReply "ACCEPTNOT")
  , (ERR_NOHIDING              , ReplyCodeInfo ErrorReply "NOHIDING")
  , (ERR_NOTFORHALFOPS         , ReplyCodeInfo ErrorReply "NOTFORHALFOPS")
  , (ERR_NEEDMOREPARAMS        , ReplyCodeInfo ErrorReply "NEEDMOREPARAMS")
  , (ERR_ALREADYREGISTERED     , ReplyCodeInfo ErrorReply "ALREADYREGISTERED")
  , (ERR_NOPERMFORHOST         , ReplyCodeInfo ErrorReply "NOPERMFORHOST")
  , (ERR_PASSWDMISMATCH        , ReplyCodeInfo ErrorReply "PASSWDMISMATCH")
  , (ERR_YOUREBANNEDCREEP      , ReplyCodeInfo ErrorReply "YOUREBANNEDCREEP")
  , (ERR_YOUWILLBEBANNED       , ReplyCodeInfo ErrorReply "YOUWILLBEBANNED")
  , (ERR_KEYSET                , ReplyCodeInfo ErrorReply "KEYSET")
  , (ERR_INVALIDUSERNAME       , ReplyCodeInfo ErrorReply "INVALIDUSERNAME")
  , (ERR_ONLYSERVERSCANCHANGE  , ReplyCodeInfo ErrorReply "ONLYSERVERSCANCHANGE")
  , (ERR_LINKSET               , ReplyCodeInfo ErrorReply "LINKSET")
  , (ERR_LINKCHANNEL           , ReplyCodeInfo ErrorReply "LINKCHANNEL")
  , (ERR_CHANNELISFULL         , ReplyCodeInfo ErrorReply "CHANNELISFULL")
  , (ERR_UNKNOWNMODE           , ReplyCodeInfo ErrorReply "UNKNOWNMODE")
  , (ERR_INVITEONLYCHAN        , ReplyCodeInfo ErrorReply "INVITEONLYCHAN")
  , (ERR_BANNEDFROMCHAN        , ReplyCodeInfo ErrorReply "BANNEDFROMCHAN")
  , (ERR_BADCHANNELKEY         , ReplyCodeInfo ErrorReply "BADCHANNELKEY")
  , (ERR_BADCHANMASK           , ReplyCodeInfo ErrorReply "BADCHANMASK")
  , (ERR_NEEDREGGEDNICK        , ReplyCodeInfo ErrorReply "NEEDREGGEDNICK")
  , (ERR_BANLISTFULL           , ReplyCodeInfo ErrorReply "BANLISTFULL")
  , (ERR_BADCHANNAME           , ReplyCodeInfo ErrorReply "BADCHANNAME")
  , (ERR_THROTTLE              , ReplyCodeInfo ErrorReply "THROTTLE")
  , (ERR_NOPRIVILEGES          , ReplyCodeInfo ErrorReply "NOPRIVILEGES")
  , (ERR_CHANOPRIVSNEEDED      , ReplyCodeInfo ErrorReply "CHANOPRIVSNEEDED")
  , (ERR_CANTKILLSERVER        , ReplyCodeInfo ErrorReply "CANTKILLSERVER")
  , (ERR_ISCHANSERVICE         , ReplyCodeInfo ErrorReply "ISCHANSERVICE")
  , (ERR_BANNEDNICK            , ReplyCodeInfo ErrorReply "BANNEDNICK")
  , (ERR_NONONREG              , ReplyCodeInfo ErrorReply "NONONREG")
  , (ERR_TSLESSCHAN            , ReplyCodeInfo ErrorReply "TSLESSCHAN")
  , (ERR_VOICENEEDED           , ReplyCodeInfo ErrorReply "VOICENEEDED")
  , (ERR_NOOPERHOST            , ReplyCodeInfo ErrorReply "NOOPERHOST")
  , (ERR_NOSERVICEHOST         , ReplyCodeInfo ErrorReply "NOSERVICEHOST")
  , (ERR_NOFEATURE             , ReplyCodeInfo ErrorReply "NOFEATURE")
  , (ERR_OWNMODE               , ReplyCodeInfo ErrorReply "OWNMODE")
  , (ERR_BADLOGTYPE            , ReplyCodeInfo ErrorReply "BADLOGTYPE")
  , (ERR_BADLOGSYS             , ReplyCodeInfo ErrorReply "BADLOGSYS")
  , (ERR_BADLOGVALUE           , ReplyCodeInfo ErrorReply "BADLOGVALUE")
  , (ERR_ISOPERLCHAN           , ReplyCodeInfo ErrorReply "ISOPERLCHAN")
  , (ERR_CHANOWNPRIVNEEDED     , ReplyCodeInfo ErrorReply "CHANOWNPRIVNEEDED")
  , (ERR_UMODEUNKNOWNFLAG      , ReplyCodeInfo ErrorReply "UMODEUNKNOWNFLAG")
  , (ERR_USERSDONTMATCH        , ReplyCodeInfo ErrorReply "USERSDONTMATCH")
  , (ERR_GHOSTEDCLIENT         , ReplyCodeInfo ErrorReply "GHOSTEDCLIENT")
  , (ERR_USERNOTONSERV         , ReplyCodeInfo ErrorReply "USERNOTONSERV")
  , (ERR_SILELISTFULL          , ReplyCodeInfo ErrorReply "SILELISTFULL")
  , (ERR_TOOMANYWATCH          , ReplyCodeInfo ErrorReply "TOOMANYWATCH")
  , (ERR_WRONGPONG             , ReplyCodeInfo ErrorReply "WRONGPONG")
  , (ERR_BADEXPIRE             , ReplyCodeInfo ErrorReply "BADEXPIRE")
  , (ERR_DONTCHEAT             , ReplyCodeInfo ErrorReply "DONTCHEAT")
  , (ERR_DISABLED              , ReplyCodeInfo ErrorReply "DISABLED")
  , (ERR_NOINVITE              , ReplyCodeInfo ErrorReply "NOINVITE")
  , (ERR_LONGMASK              , ReplyCodeInfo ErrorReply "LONGMASK")
  , (ERR_ADMONLY               , ReplyCodeInfo ErrorReply "ADMONLY")
  , (ERR_TOOMANYUSERS          , ReplyCodeInfo ErrorReply "TOOMANYUSERS")
  , (ERR_OPERONLY              , ReplyCodeInfo ErrorReply "OPERONLY")
  , (ERR_MASKTOOWIDE           , ReplyCodeInfo ErrorReply "MASKTOOWIDE")
  , (ERR_WHOTRUNC              , ReplyCodeInfo ErrorReply "WHOTRUNC")
  , (ERR_LISTSYNTAX            , ReplyCodeInfo ErrorReply "LISTSYNTAX")
  , (ERR_WHOSYNTAX             , ReplyCodeInfo ErrorReply "WHOSYNTAX")
  , (ERR_WHOLIMEXCEED          , ReplyCodeInfo ErrorReply "WHOLIMEXCEED")
  , (ERR_HELPNOTFOUND          , ReplyCodeInfo ErrorReply "HELPNOTFOUND")
  , (ERR_REMOTEPFX             , ReplyCodeInfo ErrorReply "REMOTEPFX")
  , (ERR_PFXUNROUTABLE         , ReplyCodeInfo ErrorReply "PFXUNROUTABLE")
  , (ERR_BADHOSTMASK           , ReplyCodeInfo ErrorReply "BADHOSTMASK")
  , (ERR_HOSTUNAVAIL           , ReplyCodeInfo ErrorReply "HOSTUNAVAIL")
  , (ERR_USINGSLINE            , ReplyCodeInfo ErrorReply "USINGSLINE")
  , (ERR_STATSSLINE            , ReplyCodeInfo ErrorReply "STATSSLINE")
  , (RPL_LOGON                 , ReplyCodeInfo CommandReply "LOGON")
  , (RPL_LOGOFF                , ReplyCodeInfo CommandReply "LOGOFF")
  , (RPL_WATCHOFF              , ReplyCodeInfo CommandReply "WATCHOFF")
  , (RPL_WATCHSTAT             , ReplyCodeInfo CommandReply "WATCHSTAT")
  , (RPL_NOWON                 , ReplyCodeInfo CommandReply "NOWON")
  , (RPL_NOWOFF                , ReplyCodeInfo CommandReply "NOWOFF")
  , (RPL_WATCHLIST             , ReplyCodeInfo CommandReply "WATCHLIST")
  , (RPL_ENDOFWATCHLIST        , ReplyCodeInfo CommandReply "ENDOFWATCHLIST")
  , (RPL_WATCHCLEAR            , ReplyCodeInfo CommandReply "WATCHCLEAR")
  , (RPL_ISOPER                , ReplyCodeInfo CommandReply "ISOPER")
  , (RPL_ISLOCOP               , ReplyCodeInfo CommandReply "ISLOCOP")
  , (RPL_ISNOTOPER             , ReplyCodeInfo CommandReply "ISNOTOPER")
  , (RPL_ENDOFISOPER           , ReplyCodeInfo CommandReply "ENDOFISOPER")
  , (RPL_DCCSTATUS             , ReplyCodeInfo CommandReply "DCCSTATUS")
  , (RPL_DCCLIST               , ReplyCodeInfo CommandReply "DCCLIST")
  , (RPL_ENDOFDCCLIST          , ReplyCodeInfo CommandReply "ENDOFDCCLIST")
  , (RPL_WHOWASHOST            , ReplyCodeInfo CommandReply "WHOWASHOST")
  , (RPL_DCCINFO               , ReplyCodeInfo CommandReply "DCCINFO")
  , (RPL_RULES                 , ReplyCodeInfo CommandReply "RULES")
  , (RPL_ENDOFO                , ReplyCodeInfo CommandReply "ENDOFO")
  , (RPL_SETTINGS              , ReplyCodeInfo CommandReply "SETTINGS")
  , (RPL_ENDOFSETTINGS         , ReplyCodeInfo CommandReply "ENDOFSETTINGS")
  , (RPL_DUMPING               , ReplyCodeInfo CommandReply "DUMPING")
  , (RPL_DUMPRPL               , ReplyCodeInfo CommandReply "DUMPRPL")
  , (RPL_EODUMP                , ReplyCodeInfo CommandReply "EODUMP")
  , (RPL_TRACEROUTE_HOP        , ReplyCodeInfo CommandReply "TRACEROUTE_HOP")
  , (RPL_TRACEROUTE_START      , ReplyCodeInfo CommandReply "TRACEROUTE_START")
  , (RPL_MODECHANGEWARN        , ReplyCodeInfo CommandReply "MODECHANGEWARN")
  , (RPL_CHANREDIR             , ReplyCodeInfo CommandReply "CHANREDIR")
  , (RPL_SERVMODEIS            , ReplyCodeInfo CommandReply "SERVMODEIS")
  , (RPL_OTHERUMODEIS          , ReplyCodeInfo CommandReply "OTHERUMODEIS")
  , (RPL_ENDOF_GENERIC         , ReplyCodeInfo CommandReply "ENDOF_GENERIC")
  , (RPL_WHOWASDETAILS         , ReplyCodeInfo CommandReply "WHOWASDETAILS")
  , (RPL_WHOISSECURE           , ReplyCodeInfo CommandReply "WHOISSECURE")
  , (RPL_UNKNOWNMODES          , ReplyCodeInfo CommandReply "UNKNOWNMODES")
  , (RPL_CANNOTSETMODES        , ReplyCodeInfo CommandReply "CANNOTSETMODES")
  , (RPL_LUSERSTAFF            , ReplyCodeInfo CommandReply "LUSERSTAFF")
  , (RPL_TIMEONSERVERIS        , ReplyCodeInfo CommandReply "TIMEONSERVERIS")
  , (RPL_NETWORKS              , ReplyCodeInfo CommandReply "NETWORKS")
  , (RPL_YOURLANGUAGEIS        , ReplyCodeInfo CommandReply "YOURLANGUAGEIS")
  , (RPL_LANGUAGE              , ReplyCodeInfo CommandReply "LANGUAGE")
  , (RPL_WHOISSTAFF            , ReplyCodeInfo CommandReply "WHOISSTAFF")
  , (RPL_WHOISLANGUAGE         , ReplyCodeInfo CommandReply "WHOISLANGUAGE")
  , (RPL_MODLIST               , ReplyCodeInfo CommandReply "MODLIST")
  , (RPL_ENDOFMODLIST          , ReplyCodeInfo CommandReply "ENDOFMODLIST")
  , (RPL_HELPSTART             , ReplyCodeInfo CommandReply "HELPSTART")
  , (RPL_HELPTXT               , ReplyCodeInfo CommandReply "HELPTXT")
  , (RPL_ENDOFHELP             , ReplyCodeInfo CommandReply "ENDOFHELP")
  , (ERR_TARGCHANGE            , ReplyCodeInfo ErrorReply "TARGCHANGE")
  , (RPL_ETRACEFULL            , ReplyCodeInfo CommandReply "ETRACEFULL")
  , (RPL_ETRACE                , ReplyCodeInfo CommandReply "ETRACE")
  , (RPL_KNOCK                 , ReplyCodeInfo CommandReply "KNOCK")
  , (RPL_KNOCKDLVR             , ReplyCodeInfo CommandReply "KNOCKDLVR")
  , (ERR_TOOMANYKNOCK          , ReplyCodeInfo ErrorReply "TOOMANYKNOCK")
  , (ERR_CHANOPEN              , ReplyCodeInfo ErrorReply "CHANOPEN")
  , (ERR_KNOCKONCHAN           , ReplyCodeInfo ErrorReply "KNOCKONCHAN")
  , (ERR_KNOCKDISABLED         , ReplyCodeInfo ErrorReply "KNOCKDISABLED")
  , (RPL_TARGUMODEG            , ReplyCodeInfo CommandReply "TARGUMODEG")
  , (RPL_TARGNOTIFY            , ReplyCodeInfo CommandReply "TARGNOTIFY")
  , (RPL_UMODEGMSG             , ReplyCodeInfo CommandReply "UMODEGMSG")
  , (RPL_OMOTDSTART            , ReplyCodeInfo CommandReply "OMOTDSTART")
  , (RPL_OMOTD                 , ReplyCodeInfo CommandReply "OMOTD")
  , (RPL_ENDOFOMOTD            , ReplyCodeInfo CommandReply "ENDOFOMOTD")
  , (ERR_NOPRIVS               , ReplyCodeInfo ErrorReply "NOPRIVS")
  , (RPL_TESTMASK              , ReplyCodeInfo CommandReply "TESTMASK")
  , (RPL_TESTLINE              , ReplyCodeInfo CommandReply "TESTLINE")
  , (RPL_NOTESTLINE            , ReplyCodeInfo CommandReply "NOTESTLINE")
  , (RPL_QUIETLIST             , ReplyCodeInfo CommandReply "QUIETLIST")
  , (RPL_ENDOFQUIETLIST        , ReplyCodeInfo CommandReply "ENDOFQUIETLIST")
  , (RPL_MONONLINE             , ReplyCodeInfo CommandReply "MONONLINE")
  , (RPL_MONOFFLINE            , ReplyCodeInfo CommandReply "MONOFFLINE")
  , (RPL_MONLIST               , ReplyCodeInfo CommandReply "MONLIST")
  , (RPL_ENDOFMONLIST          , ReplyCodeInfo CommandReply "ENDOFMONLIST")
  , (ERR_MONLISTFULL           , ReplyCodeInfo ErrorReply "MONLISTFULL")
  , (RPL_RSACHALLENGE2         , ReplyCodeInfo CommandReply "RSACHALLENGE2")
  , (RPL_ENDOFRSACHALLENGE2    , ReplyCodeInfo CommandReply "ENDOFRSACHALLENGE2")
  , (ERR_MLOCKRESTRICTED       , ReplyCodeInfo ErrorReply "MLOCKRESTRICTED")
  , (RPL_SCANMATCHED           , ReplyCodeInfo CommandReply "SCANMATCHED")
  , (RPL_SCANUMODES            , ReplyCodeInfo CommandReply "SCANUMODES")
  , (RPL_XINFO                 , ReplyCodeInfo CommandReply "XINFO")
  , (RPL_XINFOSTART            , ReplyCodeInfo CommandReply "XINFOSTART")
  , (RPL_XINFOEND              , ReplyCodeInfo CommandReply "XINFOEND")
  , (RPL_LOGGEDIN              , ReplyCodeInfo CommandReply "LOGGEDIN")
  , (RPL_LOGGEDOUT             , ReplyCodeInfo CommandReply "LOGGEDOUT")
  , (RPL_NICKLOCKED            , ReplyCodeInfo CommandReply "NICKLOCKED")
  , (RPL_SASLSUCCESS           , ReplyCodeInfo CommandReply "SASLSUCCESS")
  , (RPL_SASLFAIL              , ReplyCodeInfo CommandReply "SASLFAIL")
  , (RPL_SASLTOOLONG           , ReplyCodeInfo CommandReply "SASLTOOLONG")
  , (RPL_SASLABORTED           , ReplyCodeInfo CommandReply "SASLABORTED")
  , (RPL_SASLALREADY           , ReplyCodeInfo CommandReply "SASLALREADY")
  , (RPL_SASLMECHS             , ReplyCodeInfo CommandReply "SASLMECHS")
  , (ERR_CANNOTDOCOMMAND       , ReplyCodeInfo ErrorReply "CANNOTDOCOMMAND")
  , (ERR_CANNOTCHANGEUMODE     , ReplyCodeInfo ErrorReply "CANNOTCHANGEUMODE")
  , (ERR_CANNOTCHANGECHANMODE  , ReplyCodeInfo ErrorReply "CANNOTCHANGECHANMODE")
  , (ERR_CANNOTCHANGESERVERMODE, ReplyCodeInfo ErrorReply "CANNOTCHANGESERVERMODE")
  , (ERR_CANNOTSENDTONICK      , ReplyCodeInfo ErrorReply "CANNOTSENDTONICK")
  , (ERR_UNKNOWNSERVERMODE     , ReplyCodeInfo ErrorReply "UNKNOWNSERVERMODE")
  , (ERR_SERVERMODELOCK        , ReplyCodeInfo ErrorReply "SERVERMODELOCK")
  , (ERR_BADCHARENCODING       , ReplyCodeInfo ErrorReply "BADCHARENCODING")
  , (ERR_TOOMANYLANGUAGES      , ReplyCodeInfo ErrorReply "TOOMANYLANGUAGES")
  , (ERR_NOLANGUAGE            , ReplyCodeInfo ErrorReply "NOLANGUAGE")
  , (ERR_TEXTTOOSHORT          , ReplyCodeInfo ErrorReply "TEXTTOOSHORT")
  , (ERR_NUMERIC_ERR           , ReplyCodeInfo ErrorReply "NUMERIC_ERR")
  ]
