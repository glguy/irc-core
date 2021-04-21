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

-- | Shows number
instance Show ReplyCode where
  showsPrec p (ReplyCode x) = showsPrec p x

-- | Reads only the number
instance Read ReplyCode where
  readsPrec p str = [ (ReplyCode x, xs) | (x,xs) <- readsPrec p str ]

-- | Categories for reply codes
data ReplyType
  = ClientServerReply -- ^ 0-99 Messages between client and server
  | CommandReply      -- ^ 200-399 Responses to commands
  | ErrorReply        -- ^ 400-599 Errors
  | UnknownReply      -- ^ Uncategorized
  deriving (Eq, Ord, Read, Show)

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
pattern RPL_REMOTESUPPORT           = ReplyCode 105 -- ircd-seven unnamed code
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
pattern RPL_WHOISMODES              = ReplyCode 379
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
pattern ERR_INVALIDCAPCMD           = ReplyCode 410
pattern ERR_NORECIPIENT             = ReplyCode 411
pattern ERR_NOTEXTTOSEND            = ReplyCode 412
pattern ERR_NOTOPLEVEL              = ReplyCode 413
pattern ERR_WILDTOPLEVEL            = ReplyCode 414
pattern ERR_MSGNEEDREGGEDNICK       = ReplyCode 415
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
pattern RPL_STARTTLS                = ReplyCode 670
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
pattern ERR_STARTTLS                = ReplyCode 691
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
pattern RPL_TESTMASKGECOS           = ReplyCode 727
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

-- | Information describing the category and human decipherable name of a
-- reply.
data ReplyCodeInfo = ReplyCodeInfo
  { replyCodeType :: !ReplyType -- ^ category
  , replyCodeText :: !Text      -- ^ human-decipherable name
  }
  deriving (Eq, Ord, Show, Read)

-- | Compute information for a reply code
replyCodeInfo :: ReplyCode -> ReplyCodeInfo
replyCodeInfo (ReplyCode w) =
  case replyCodeInfoTable Vector.!? i of
    Nothing -> defaultReplyCodeInfo i
    Just info -> info
  where
    i = fromIntegral w

-- | Categorize a reply code using the unknown category and simply showing
-- the reply code's number as its name.
defaultReplyCodeInfo :: Int -> ReplyCodeInfo
defaultReplyCodeInfo = ReplyCodeInfo UnknownReply . Text.pack . show

-- | Information about reply codes as derived from Freenode's ircd-seven.
replyCodeInfoTable :: Vector ReplyCodeInfo
replyCodeInfoTable
  = Vector.accumulate
      (\_def new -> new)
      (Vector.generate 1000 defaultReplyCodeInfo)
  $ fmap (\(ReplyCode code,info) -> (fromIntegral code, info))
  $ Vector.fromList
  [ (RPL_WELCOME               , ReplyCodeInfo ClientServerReply "welcome")
  , (RPL_YOURHOST              , ReplyCodeInfo ClientServerReply "your-host")
  , (RPL_CREATED               , ReplyCodeInfo ClientServerReply "created")
  , (RPL_MYINFO                , ReplyCodeInfo ClientServerReply "my-info")
  , (RPL_ISUPPORT              , ReplyCodeInfo ClientServerReply "isupport")
  , (RPL_SNOMASK               , ReplyCodeInfo ClientServerReply "sno-mask")
  , (RPL_STATMEMTOT            , ReplyCodeInfo ClientServerReply "stat-mem-tot")
  , (RPL_REDIR                 , ReplyCodeInfo ClientServerReply "redir")
  , (RPL_YOURCOOKIE            , ReplyCodeInfo ClientServerReply "your-cookie")
  , (RPL_MAP                   , ReplyCodeInfo ClientServerReply "map")
  , (RPL_MAPEND                , ReplyCodeInfo ClientServerReply "map-end")
  , (RPL_YOURID                , ReplyCodeInfo ClientServerReply "your-id")
  , (RPL_SAVENICK              , ReplyCodeInfo ClientServerReply "save-nick")
  , (RPL_ATTEMPTINGJUNC        , ReplyCodeInfo ClientServerReply "attempting-junc")
  , (RPL_ATTEMPTINGREROUTE     , ReplyCodeInfo ClientServerReply "attempting-reroute")
  , (RPL_REMOTESUPPORT         , ReplyCodeInfo ClientServerReply "remote-support")
  , (RPL_TRACELINK             , ReplyCodeInfo CommandReply "trace-link")
  , (RPL_TRACECONNECTING       , ReplyCodeInfo CommandReply "trace-connecting")
  , (RPL_TRACEHANDSHAKE        , ReplyCodeInfo CommandReply "trace-handshake")
  , (RPL_TRACEUNKNOWN          , ReplyCodeInfo CommandReply "trace-unknown")
  , (RPL_TRACEOPERATOR         , ReplyCodeInfo CommandReply "trace-operator")
  , (RPL_TRACEUSER             , ReplyCodeInfo CommandReply "trace-user")
  , (RPL_TRACESERVER           , ReplyCodeInfo CommandReply "trace-server")
  , (RPL_TRACESERVICE          , ReplyCodeInfo CommandReply "trace-service")
  , (RPL_TRACENEWTYPE          , ReplyCodeInfo CommandReply "trace-newtype")
  , (RPL_TRACECLASS            , ReplyCodeInfo CommandReply "trace-class")
  , (RPL_TRACERECONNECT        , ReplyCodeInfo CommandReply "trace-reconnect")
  , (RPL_STATS                 , ReplyCodeInfo CommandReply "stats")
  , (RPL_STATSLINKINFO         , ReplyCodeInfo CommandReply "stats-linkinfo")
  , (RPL_STATSCOMMANDS         , ReplyCodeInfo CommandReply "stats-commands")
  , (RPL_STATSCLINE            , ReplyCodeInfo CommandReply "stats-cline")
  , (RPL_STATSNLINE            , ReplyCodeInfo CommandReply "stats-nline")
  , (RPL_STATSILINE            , ReplyCodeInfo CommandReply "stats-iline")
  , (RPL_STATSKLINE            , ReplyCodeInfo CommandReply "stats-kline")
  , (RPL_STATSQLINE            , ReplyCodeInfo CommandReply "stats-qline")
  , (RPL_STATSYLINE            , ReplyCodeInfo CommandReply "stats-yline")
  , (RPL_ENDOFSTATS            , ReplyCodeInfo CommandReply "end-of-stats")
  , (RPL_STATSPLINE            , ReplyCodeInfo CommandReply "stats-pline")
  , (RPL_UMODEIS               , ReplyCodeInfo CommandReply "umode-is")
  , (RPL_SQLINE_NICK           , ReplyCodeInfo CommandReply "sqline-nick")
  , (RPL_STATSDLINE            , ReplyCodeInfo CommandReply "stats-dline")
  , (RPL_STATSCOUNT            , ReplyCodeInfo CommandReply "stats-count")
  , (RPL_SERVICEINFO           , ReplyCodeInfo CommandReply "service-info")
  , (RPL_ENDOFSERVICES         , ReplyCodeInfo CommandReply "end-of-services")
  , (RPL_SERVICE               , ReplyCodeInfo CommandReply "service")
  , (RPL_SERVLIST              , ReplyCodeInfo CommandReply "serv-list")
  , (RPL_SERVLISTEND           , ReplyCodeInfo CommandReply "serv-list-end")
  , (RPL_STATSVERBOSE          , ReplyCodeInfo CommandReply "stats-verbose")
  , (RPL_STATSIAUTH            , ReplyCodeInfo CommandReply "stats-iauth")
  , (RPL_STATSLLINE            , ReplyCodeInfo CommandReply "stats-lline")
  , (RPL_STATSUPTIME           , ReplyCodeInfo CommandReply "stats-uptime")
  , (RPL_STATSOLINE            , ReplyCodeInfo CommandReply "stats-oline")
  , (RPL_STATSHLINE            , ReplyCodeInfo CommandReply "stats-hline")
  , (RPL_STATSSLINE            , ReplyCodeInfo CommandReply "stats-sline")
  , (RPL_STATSPING             , ReplyCodeInfo CommandReply "stats-ping")
  , (RPL_STATSXLINE            , ReplyCodeInfo CommandReply "stats-xline")
  , (RPL_STATSULINE            , ReplyCodeInfo CommandReply "stats-uline")
  , (RPL_STATSDEBUG            , ReplyCodeInfo CommandReply "stats-debug")
  , (RPL_STATSCONN             , ReplyCodeInfo CommandReply "stats-conn")
  , (RPL_LUSERCLIENT           , ReplyCodeInfo CommandReply "luser-client")
  , (RPL_LUSEROP               , ReplyCodeInfo CommandReply "luser-op")
  , (RPL_LUSERUNKNOWN          , ReplyCodeInfo CommandReply "luser-unknown")
  , (RPL_LUSERCHANNELS         , ReplyCodeInfo CommandReply "luser-channels")
  , (RPL_LUSERME               , ReplyCodeInfo CommandReply "luser-me")
  , (RPL_ADMINME               , ReplyCodeInfo CommandReply "admin-me")
  , (RPL_ADMINLOC1             , ReplyCodeInfo CommandReply "admin-loc1")
  , (RPL_ADMINLOC2             , ReplyCodeInfo CommandReply "admin-loc2")
  , (RPL_ADMINEMAIL            , ReplyCodeInfo CommandReply "admin-email")
  , (RPL_TRACELOG              , ReplyCodeInfo CommandReply "trace-log")
  , (RPL_ENDOFTRACE            , ReplyCodeInfo CommandReply "end-of-trace")
  , (RPL_LOAD2HI               , ReplyCodeInfo CommandReply "load-too-hi")
  , (RPL_LOCALUSERS            , ReplyCodeInfo CommandReply "local-users")
  , (RPL_GLOBALUSERS           , ReplyCodeInfo CommandReply "global-users")
  , (RPL_START_NETSTAT         , ReplyCodeInfo CommandReply "start-netstat")
  , (RPL_NETSTAT               , ReplyCodeInfo CommandReply "netstat")
  , (RPL_END_NETSTAT           , ReplyCodeInfo CommandReply "end-netstat")
  , (RPL_PRIVS                 , ReplyCodeInfo CommandReply "privs")
  , (RPL_SILELIST              , ReplyCodeInfo CommandReply "sile-list")
  , (RPL_ENDOFSILELIST         , ReplyCodeInfo CommandReply "end-of-sile-list")
  , (RPL_NOTIFY                , ReplyCodeInfo CommandReply "notify")
  , (RPL_ENDNOTIFY             , ReplyCodeInfo CommandReply "end-notify")
  , (RPL_STATSDELTA            , ReplyCodeInfo CommandReply "stats-delta")
  , (RPL_WHOISCERTFP           , ReplyCodeInfo CommandReply "whois-certfp")
  , (RPL_VCHANLIST             , ReplyCodeInfo CommandReply "vchan-list")
  , (RPL_VCHANHELP             , ReplyCodeInfo CommandReply "vchan-help")
  , (RPL_GLIST                 , ReplyCodeInfo CommandReply "glist")
  , (RPL_ACCEPTLIST            , ReplyCodeInfo CommandReply "accept-list")
  , (RPL_ENDOFACCEPT           , ReplyCodeInfo CommandReply "end-of-accept")
  , (RPL_ENDOFJUPELIST         , ReplyCodeInfo CommandReply "end-of-jupe-list")
  , (RPL_FEATURE               , ReplyCodeInfo CommandReply "feature")
  , (RPL_DATASTR               , ReplyCodeInfo CommandReply "datastr")
  , (RPL_END_CHANINFO          , ReplyCodeInfo CommandReply "end-chaninfo")
  , (RPL_NONE                  , ReplyCodeInfo CommandReply "none")
  , (RPL_AWAY                  , ReplyCodeInfo CommandReply "away")
  , (RPL_USERHOST              , ReplyCodeInfo CommandReply "userhost")
  , (RPL_ISON                  , ReplyCodeInfo CommandReply "ison")
  , (RPL_TEXT                  , ReplyCodeInfo CommandReply "text")
  , (RPL_UNAWAY                , ReplyCodeInfo CommandReply "unaway")
  , (RPL_NOWAWAY               , ReplyCodeInfo CommandReply "now-away")
  , (RPL_WHOISREGNICK          , ReplyCodeInfo CommandReply "whois-regnick")
  , (RPL_SUSERHOST             , ReplyCodeInfo CommandReply "suserhost")
  , (RPL_NOTIFYACTION          , ReplyCodeInfo CommandReply "notify-action")
  , (RPL_WHOISADMIN            , ReplyCodeInfo CommandReply "whois-admin")
  , (RPL_NICKTRACE             , ReplyCodeInfo CommandReply "nick-trace")
  , (RPL_WHOISSADMIN           , ReplyCodeInfo CommandReply "whois-sadmin")
  , (RPL_WHOISHELPER           , ReplyCodeInfo CommandReply "whois-helper")
  , (RPL_WHOISUSER             , ReplyCodeInfo CommandReply "whois-user")
  , (RPL_WHOISSERVER           , ReplyCodeInfo CommandReply "whois-server")
  , (RPL_WHOISOPERATOR         , ReplyCodeInfo CommandReply "whois-operator")
  , (RPL_WHOWASUSER            , ReplyCodeInfo CommandReply "whowas-user")
  , (RPL_ENDOFWHO              , ReplyCodeInfo CommandReply "end-of-who")
  , (RPL_WHOISCHANOP           , ReplyCodeInfo CommandReply "whois-chanop")
  , (RPL_WHOISIDLE             , ReplyCodeInfo CommandReply "whois-idle")
  , (RPL_ENDOFWHOIS            , ReplyCodeInfo CommandReply "end-of-whois")
  , (RPL_WHOISCHANNELS         , ReplyCodeInfo CommandReply "whois-channels")
  , (RPL_WHOISSPECIAL          , ReplyCodeInfo CommandReply "whois-special")
  , (RPL_LISTSTART             , ReplyCodeInfo CommandReply "list-start")
  , (RPL_LIST                  , ReplyCodeInfo CommandReply "list")
  , (RPL_LISTEND               , ReplyCodeInfo CommandReply "list-end")
  , (RPL_CHANNELMODEIS         , ReplyCodeInfo CommandReply "channel-mode-is")
  , (RPL_CHANNELMLOCKIS        , ReplyCodeInfo CommandReply "channel-mlock-is")
  , (RPL_NOCHANPASS            , ReplyCodeInfo CommandReply "nochanpass")
  , (RPL_CHPASSUNKNOWN         , ReplyCodeInfo CommandReply "chpass-unknown")
  , (RPL_CHANNEL_URL           , ReplyCodeInfo CommandReply "channel-url")
  , (RPL_CREATIONTIME          , ReplyCodeInfo CommandReply "creation-time")
  , (RPL_WHOISACCOUNT          , ReplyCodeInfo CommandReply "whois-account")
  , (RPL_NOTOPIC               , ReplyCodeInfo CommandReply "notopic")
  , (RPL_TOPIC                 , ReplyCodeInfo CommandReply "topic")
  , (RPL_TOPICWHOTIME          , ReplyCodeInfo CommandReply "topic-whotime")
  , (RPL_LISTUSAGE             , ReplyCodeInfo CommandReply "list-usage")
  , (RPL_COMMANDSYNTAX         , ReplyCodeInfo CommandReply "command-syntax")
  , (RPL_LISTSYNTAX            , ReplyCodeInfo CommandReply "list-syntax")
  , (RPL_WHOISACTUALLY         , ReplyCodeInfo CommandReply "whois-actually")
  , (RPL_BADCHANPASS           , ReplyCodeInfo CommandReply "bad-chanpass")
  , (RPL_INVITING              , ReplyCodeInfo CommandReply "inviting")
  , (RPL_SUMMONING             , ReplyCodeInfo CommandReply "summoning")
  , (RPL_INVITED               , ReplyCodeInfo CommandReply "invited")
  , (RPL_INVEXLIST             , ReplyCodeInfo CommandReply "invex-list")
  , (RPL_ENDOFINVEXLIST        , ReplyCodeInfo CommandReply "end-of-invex-list")
  , (RPL_EXCEPTLIST            , ReplyCodeInfo CommandReply "except-list")
  , (RPL_ENDOFEXCEPTLIST       , ReplyCodeInfo CommandReply "end-of-except-list")
  , (RPL_VERSION               , ReplyCodeInfo CommandReply "version")
  , (RPL_WHOREPLY              , ReplyCodeInfo CommandReply "who-reply")
  , (RPL_NAMREPLY              , ReplyCodeInfo CommandReply "nam-reply")
  , (RPL_WHOSPCRPL             , ReplyCodeInfo CommandReply "who-special-reply")
  , (RPL_NAMREPLY_             , ReplyCodeInfo CommandReply "nam-reply_")
  , (RPL_WHOWASREAL            , ReplyCodeInfo CommandReply "whowas-real")
  , (RPL_KILLDONE              , ReplyCodeInfo CommandReply "kill-done")
  , (RPL_CLOSING               , ReplyCodeInfo CommandReply "closing")
  , (RPL_CLOSEEND              , ReplyCodeInfo CommandReply "close-end")
  , (RPL_LINKS                 , ReplyCodeInfo CommandReply "links")
  , (RPL_ENDOFLINKS            , ReplyCodeInfo CommandReply "end-of-links")
  , (RPL_ENDOFNAMES            , ReplyCodeInfo CommandReply "end-of-names")
  , (RPL_BANLIST               , ReplyCodeInfo CommandReply "ban-list")
  , (RPL_ENDOFBANLIST          , ReplyCodeInfo CommandReply "end-of-ban-list")
  , (RPL_ENDOFWHOWAS           , ReplyCodeInfo CommandReply "end-of-whowas")
  , (RPL_INFO                  , ReplyCodeInfo CommandReply "info")
  , (RPL_MOTD                  , ReplyCodeInfo CommandReply "motd")
  , (RPL_INFOSTART             , ReplyCodeInfo CommandReply "info-start")
  , (RPL_ENDOFINFO             , ReplyCodeInfo CommandReply "end-of-info")
  , (RPL_MOTDSTART             , ReplyCodeInfo CommandReply "motd-start")
  , (RPL_ENDOFMOTD             , ReplyCodeInfo CommandReply "end-of-motd")
  , (RPL_WHOISHOST             , ReplyCodeInfo CommandReply "whois-host")
  , (RPL_WHOISMODES            , ReplyCodeInfo CommandReply "whois-modes")
  , (RPL_YOUREOPER             , ReplyCodeInfo CommandReply "youre-oper")
  , (RPL_REHASHING             , ReplyCodeInfo CommandReply "rehashing")
  , (RPL_YOURESERVICE          , ReplyCodeInfo CommandReply "youre-service")
  , (RPL_MYPORTIS              , ReplyCodeInfo CommandReply "my-port-is")
  , (RPL_NOTOPERANYMORE        , ReplyCodeInfo CommandReply "not-oper-anymore")
  , (RPL_RSACHALLENGE          , ReplyCodeInfo CommandReply "rsa-challenge")
  , (RPL_TIME                  , ReplyCodeInfo CommandReply "time")
  , (RPL_USERSSTART            , ReplyCodeInfo CommandReply "users-start")
  , (RPL_USERS                 , ReplyCodeInfo CommandReply "users")
  , (RPL_ENDOFUSERS            , ReplyCodeInfo CommandReply "end-of-users")
  , (RPL_NOUSERS               , ReplyCodeInfo CommandReply "nousers")
  , (RPL_HOSTHIDDEN            , ReplyCodeInfo CommandReply "host-hidden")
  , (ERR_UNKNOWNERROR          , ReplyCodeInfo ErrorReply "unknown-error")
  , (ERR_NOSUCHNICK            , ReplyCodeInfo ErrorReply "no-such-nick")
  , (ERR_NOSUCHSERVER          , ReplyCodeInfo ErrorReply "no-such-server")
  , (ERR_NOSUCHCHANNEL         , ReplyCodeInfo ErrorReply "no-such-channel")
  , (ERR_CANNOTSENDTOCHAN      , ReplyCodeInfo ErrorReply "cannot-send-to-chan")
  , (ERR_TOOMANYCHANNELS       , ReplyCodeInfo ErrorReply "too-many-channels")
  , (ERR_WASNOSUCHNICK         , ReplyCodeInfo ErrorReply "was-no-such-nick")
  , (ERR_TOOMANYTARGETS        , ReplyCodeInfo ErrorReply "too-many-targets")
  , (ERR_NOORIGIN              , ReplyCodeInfo ErrorReply "no-origin")
  , (ERR_INVALIDCAPCMD         , ReplyCodeInfo ErrorReply "invalid-cap-cmd")
  , (ERR_NORECIPIENT           , ReplyCodeInfo ErrorReply "no-recipient")
  , (ERR_NOTEXTTOSEND          , ReplyCodeInfo ErrorReply "no-text-to-send")
  , (ERR_NOTOPLEVEL            , ReplyCodeInfo ErrorReply "no-top-level")
  , (ERR_WILDTOPLEVEL          , ReplyCodeInfo ErrorReply "wild-top-level")
  , (ERR_MSGNEEDREGGEDNICK     , ReplyCodeInfo ErrorReply "msg-need-regged-nick")
  , (ERR_TOOMANYMATCHES        , ReplyCodeInfo ErrorReply "too-many-matches")
  , (ERR_LENGTHTRUNCATED       , ReplyCodeInfo ErrorReply "length-truncated")
  , (ERR_UNKNOWNCOMMAND        , ReplyCodeInfo ErrorReply "unknown-command")
  , (ERR_NOMOTD                , ReplyCodeInfo ErrorReply "no-motd")
  , (ERR_NOADMININFO           , ReplyCodeInfo ErrorReply "no-admin-info")
  , (ERR_FILEERROR             , ReplyCodeInfo ErrorReply "file-error")
  , (ERR_NOOPERMOTD            , ReplyCodeInfo ErrorReply "no-oper-motd")
  , (ERR_TOOMANYAWAY           , ReplyCodeInfo ErrorReply "too-many-away")
  , (ERR_EVENTNICKCHANGE       , ReplyCodeInfo ErrorReply "event-nick-change")
  , (ERR_NONICKNAMEGIVEN       , ReplyCodeInfo ErrorReply "no-nickname-given")
  , (ERR_ERRONEUSNICKNAME      , ReplyCodeInfo ErrorReply "err-no-use-nickname")
  , (ERR_NICKNAMEINUSE         , ReplyCodeInfo ErrorReply "nickname-in-use")
  , (ERR_SERVICENAMEINUSE      , ReplyCodeInfo ErrorReply "service-name-in-use")
  , (ERR_NORULES               , ReplyCodeInfo ErrorReply "no-rules")
  , (ERR_BANNICKCHANGE         , ReplyCodeInfo ErrorReply "ban-nick-change")
  , (ERR_NICKCOLLISION         , ReplyCodeInfo ErrorReply "nick-collision")
  , (ERR_UNAVAILRESOURCE       , ReplyCodeInfo ErrorReply "unavail-resource")
  , (ERR_NICKTOOFAST           , ReplyCodeInfo ErrorReply "nick-too-fast")
  , (ERR_TARGETTOOFAST         , ReplyCodeInfo ErrorReply "target-too-fast")
  , (ERR_SERVICESDOWN          , ReplyCodeInfo ErrorReply "services-down")
  , (ERR_USERNOTINCHANNEL      , ReplyCodeInfo ErrorReply "user-not-in-channel")
  , (ERR_NOTONCHANNEL          , ReplyCodeInfo ErrorReply "not-on-channel")
  , (ERR_USERONCHANNEL         , ReplyCodeInfo ErrorReply "user-on-channel")
  , (ERR_NOLOGIN               , ReplyCodeInfo ErrorReply "no-login")
  , (ERR_SUMMONDISABLED        , ReplyCodeInfo ErrorReply "summon-disabled")
  , (ERR_USERSDISABLED         , ReplyCodeInfo ErrorReply "users-disabled")
  , (ERR_NONICKCHANGE          , ReplyCodeInfo ErrorReply "no-nick-change")
  , (ERR_NOTIMPLEMENTED        , ReplyCodeInfo ErrorReply "not-implemented")
  , (ERR_NOTREGISTERED         , ReplyCodeInfo ErrorReply "not-registered")
  , (ERR_IDCOLLISION           , ReplyCodeInfo ErrorReply "id-collision")
  , (ERR_NICKLOST              , ReplyCodeInfo ErrorReply "nick-lost")
  , (ERR_HOSTILENAME           , ReplyCodeInfo ErrorReply "hostile-name")
  , (ERR_ACCEPTFULL            , ReplyCodeInfo ErrorReply "accept-full")
  , (ERR_ACCEPTEXIST           , ReplyCodeInfo ErrorReply "accept-exist")
  , (ERR_ACCEPTNOT             , ReplyCodeInfo ErrorReply "accept-not")
  , (ERR_NOHIDING              , ReplyCodeInfo ErrorReply "no-hiding")
  , (ERR_NOTFORHALFOPS         , ReplyCodeInfo ErrorReply "not-for-halfops")
  , (ERR_NEEDMOREPARAMS        , ReplyCodeInfo ErrorReply "need-more-params")
  , (ERR_ALREADYREGISTERED     , ReplyCodeInfo ErrorReply "already-registered")
  , (ERR_NOPERMFORHOST         , ReplyCodeInfo ErrorReply "no-perm-for-host")
  , (ERR_PASSWDMISMATCH        , ReplyCodeInfo ErrorReply "passwd-mismatch")
  , (ERR_YOUREBANNEDCREEP      , ReplyCodeInfo ErrorReply "youre-banned-creep")
  , (ERR_YOUWILLBEBANNED       , ReplyCodeInfo ErrorReply "you-will-be-banned")
  , (ERR_KEYSET                , ReplyCodeInfo ErrorReply "keyset")
  , (ERR_INVALIDUSERNAME       , ReplyCodeInfo ErrorReply "invalid-username")
  , (ERR_ONLYSERVERSCANCHANGE  , ReplyCodeInfo ErrorReply "only-servers-can-change")
  , (ERR_LINKSET               , ReplyCodeInfo ErrorReply "link-set")
  , (ERR_LINKCHANNEL           , ReplyCodeInfo ErrorReply "link-channel")
  , (ERR_CHANNELISFULL         , ReplyCodeInfo ErrorReply "channel-is-full")
  , (ERR_UNKNOWNMODE           , ReplyCodeInfo ErrorReply "unknown-mode")
  , (ERR_INVITEONLYCHAN        , ReplyCodeInfo ErrorReply "invite-only-chan")
  , (ERR_BANNEDFROMCHAN        , ReplyCodeInfo ErrorReply "banned-from-chan")
  , (ERR_BADCHANNELKEY         , ReplyCodeInfo ErrorReply "bad-channel-key")
  , (ERR_BADCHANMASK           , ReplyCodeInfo ErrorReply "bad-chan-mask")
  , (ERR_NEEDREGGEDNICK        , ReplyCodeInfo ErrorReply "need-regged-nick")
  , (ERR_BANLISTFULL           , ReplyCodeInfo ErrorReply "ban-list-full")
  , (ERR_BADCHANNAME           , ReplyCodeInfo ErrorReply "bad-chan-name")
  , (ERR_THROTTLE              , ReplyCodeInfo ErrorReply "throttle")
  , (ERR_NOPRIVILEGES          , ReplyCodeInfo ErrorReply "no-privileges")
  , (ERR_CHANOPRIVSNEEDED      , ReplyCodeInfo ErrorReply "chano-privs-needed")
  , (ERR_CANTKILLSERVER        , ReplyCodeInfo ErrorReply "cant-kill-server")
  , (ERR_ISCHANSERVICE         , ReplyCodeInfo ErrorReply "is-chan-service")
  , (ERR_BANNEDNICK            , ReplyCodeInfo ErrorReply "banned-nick")
  , (ERR_NONONREG              , ReplyCodeInfo ErrorReply "no-nonreg")
  , (ERR_TSLESSCHAN            , ReplyCodeInfo ErrorReply "tsless-chan")
  , (ERR_VOICENEEDED           , ReplyCodeInfo ErrorReply "voice-needed")
  , (ERR_NOOPERHOST            , ReplyCodeInfo ErrorReply "no-oper-host")
  , (ERR_NOSERVICEHOST         , ReplyCodeInfo ErrorReply "no-service-host")
  , (ERR_NOFEATURE             , ReplyCodeInfo ErrorReply "no-feature")
  , (ERR_OWNMODE               , ReplyCodeInfo ErrorReply "own-mode")
  , (ERR_BADLOGTYPE            , ReplyCodeInfo ErrorReply "bad-log-type")
  , (ERR_BADLOGSYS             , ReplyCodeInfo ErrorReply "bad-log-sys")
  , (ERR_BADLOGVALUE           , ReplyCodeInfo ErrorReply "bad-log-value")
  , (ERR_ISOPERLCHAN           , ReplyCodeInfo ErrorReply "is-oper-lchan")
  , (ERR_CHANOWNPRIVNEEDED     , ReplyCodeInfo ErrorReply "chan-own-priv-needed")
  , (ERR_UMODEUNKNOWNFLAG      , ReplyCodeInfo ErrorReply "umode-unknown-flag")
  , (ERR_USERSDONTMATCH        , ReplyCodeInfo ErrorReply "users-dont-match")
  , (ERR_GHOSTEDCLIENT         , ReplyCodeInfo ErrorReply "ghosted-client")
  , (ERR_USERNOTONSERV         , ReplyCodeInfo ErrorReply "user-not-on-serv")
  , (ERR_SILELISTFULL          , ReplyCodeInfo ErrorReply "sile-list-full")
  , (ERR_TOOMANYWATCH          , ReplyCodeInfo ErrorReply "too-many-watch")
  , (ERR_WRONGPONG             , ReplyCodeInfo ErrorReply "wrong-pong")
  , (ERR_BADEXPIRE             , ReplyCodeInfo ErrorReply "bad-expire")
  , (ERR_DONTCHEAT             , ReplyCodeInfo ErrorReply "dont-cheat")
  , (ERR_DISABLED              , ReplyCodeInfo ErrorReply "disabled")
  , (ERR_NOINVITE              , ReplyCodeInfo ErrorReply "no-invite")
  , (ERR_LONGMASK              , ReplyCodeInfo ErrorReply "long-mask")
  , (ERR_ADMONLY               , ReplyCodeInfo ErrorReply "adm-only")
  , (ERR_TOOMANYUSERS          , ReplyCodeInfo ErrorReply "too-many-users")
  , (ERR_OPERONLY              , ReplyCodeInfo ErrorReply "oper-only")
  , (ERR_MASKTOOWIDE           , ReplyCodeInfo ErrorReply "mask-too-wide")
  , (ERR_WHOTRUNC              , ReplyCodeInfo ErrorReply "who-trunc")
  , (ERR_LISTSYNTAX            , ReplyCodeInfo ErrorReply "list-syntax")
  , (ERR_WHOSYNTAX             , ReplyCodeInfo ErrorReply "whosyntax")
  , (ERR_WHOLIMEXCEED          , ReplyCodeInfo ErrorReply "wholimexceed")
  , (ERR_HELPNOTFOUND          , ReplyCodeInfo ErrorReply "help-not-found")
  , (ERR_REMOTEPFX             , ReplyCodeInfo ErrorReply "remote-pfx")
  , (ERR_PFXUNROUTABLE         , ReplyCodeInfo ErrorReply "pfx-unroutable")
  , (ERR_BADHOSTMASK           , ReplyCodeInfo ErrorReply "bad-hostmask")
  , (ERR_HOSTUNAVAIL           , ReplyCodeInfo ErrorReply "host-unavail")
  , (ERR_USINGSLINE            , ReplyCodeInfo ErrorReply "using-sline")
  , (ERR_STATSSLINE            , ReplyCodeInfo ErrorReply "stats-sline")
  , (RPL_LOGON                 , ReplyCodeInfo CommandReply "logon")
  , (RPL_LOGOFF                , ReplyCodeInfo CommandReply "logoff")
  , (RPL_WATCHOFF              , ReplyCodeInfo CommandReply "watch-off")
  , (RPL_WATCHSTAT             , ReplyCodeInfo CommandReply "watch-stat")
  , (RPL_NOWON                 , ReplyCodeInfo CommandReply "now-on")
  , (RPL_NOWOFF                , ReplyCodeInfo CommandReply "now-off")
  , (RPL_WATCHLIST             , ReplyCodeInfo CommandReply "watch-list")
  , (RPL_ENDOFWATCHLIST        , ReplyCodeInfo CommandReply "end-of-watch-list")
  , (RPL_WATCHCLEAR            , ReplyCodeInfo CommandReply "watch-clear")
  , (RPL_ISOPER                , ReplyCodeInfo CommandReply "is-oper")
  , (RPL_ISLOCOP               , ReplyCodeInfo CommandReply "is-loc-op")
  , (RPL_ISNOTOPER             , ReplyCodeInfo CommandReply "is-not-oper")
  , (RPL_ENDOFISOPER           , ReplyCodeInfo CommandReply "end-of-is-oper")
  , (RPL_DCCSTATUS             , ReplyCodeInfo CommandReply "dcc-status")
  , (RPL_DCCLIST               , ReplyCodeInfo CommandReply "dcc-list")
  , (RPL_ENDOFDCCLIST          , ReplyCodeInfo CommandReply "end-of-dcc-list")
  , (RPL_WHOWASHOST            , ReplyCodeInfo CommandReply "whowas-host")
  , (RPL_DCCINFO               , ReplyCodeInfo CommandReply "dcc-info")
  , (RPL_RULES                 , ReplyCodeInfo CommandReply "rules")
  , (RPL_ENDOFO                , ReplyCodeInfo CommandReply "end-of-o")
  , (RPL_SETTINGS              , ReplyCodeInfo CommandReply "settings")
  , (RPL_ENDOFSETTINGS         , ReplyCodeInfo CommandReply "end-of-settings")
  , (RPL_DUMPING               , ReplyCodeInfo CommandReply "dumping")
  , (RPL_DUMPRPL               , ReplyCodeInfo CommandReply "dump-rpl")
  , (RPL_EODUMP                , ReplyCodeInfo CommandReply "eodump")
  , (RPL_TRACEROUTE_HOP        , ReplyCodeInfo CommandReply "traceroute-hop")
  , (RPL_TRACEROUTE_START      , ReplyCodeInfo CommandReply "traceroute-start")
  , (RPL_MODECHANGEWARN        , ReplyCodeInfo CommandReply "mode-change-warn")
  , (RPL_CHANREDIR             , ReplyCodeInfo CommandReply "chan-redir")
  , (RPL_SERVMODEIS            , ReplyCodeInfo CommandReply "serv-mode-is")
  , (RPL_OTHERUMODEIS          , ReplyCodeInfo CommandReply "other-umode-is")
  , (RPL_ENDOF_GENERIC         , ReplyCodeInfo CommandReply "end-of-generic")
  , (RPL_STARTTLS              , ReplyCodeInfo CommandReply "start-tls")
  , (RPL_WHOISSECURE           , ReplyCodeInfo CommandReply "whois-secure")
  , (RPL_UNKNOWNMODES          , ReplyCodeInfo CommandReply "unknown-modes")
  , (RPL_CANNOTSETMODES        , ReplyCodeInfo CommandReply "cannot-set-modes")
  , (RPL_LUSERSTAFF            , ReplyCodeInfo CommandReply "luser-staff")
  , (RPL_TIMEONSERVERIS        , ReplyCodeInfo CommandReply "time-on-server-is")
  , (RPL_NETWORKS              , ReplyCodeInfo CommandReply "networks")
  , (RPL_YOURLANGUAGEIS        , ReplyCodeInfo CommandReply "your-language-is")
  , (RPL_LANGUAGE              , ReplyCodeInfo CommandReply "language")
  , (RPL_WHOISSTAFF            , ReplyCodeInfo CommandReply "whois-staff")
  , (RPL_WHOISLANGUAGE         , ReplyCodeInfo CommandReply "who-is-language")
  , (ERR_STARTTLS              , ReplyCodeInfo CommandReply "err-start-tls")
  , (RPL_MODLIST               , ReplyCodeInfo CommandReply "mod-list")
  , (RPL_ENDOFMODLIST          , ReplyCodeInfo CommandReply "end-of-modlist")
  , (RPL_HELPSTART             , ReplyCodeInfo CommandReply "help-start")
  , (RPL_HELPTXT               , ReplyCodeInfo CommandReply "help-txt")
  , (RPL_ENDOFHELP             , ReplyCodeInfo CommandReply "end-of-help")
  , (ERR_TARGCHANGE            , ReplyCodeInfo ErrorReply "targ-change")
  , (RPL_ETRACEFULL            , ReplyCodeInfo CommandReply "etrace-full")
  , (RPL_ETRACE                , ReplyCodeInfo CommandReply "etrace")
  , (RPL_KNOCK                 , ReplyCodeInfo CommandReply "knock")
  , (RPL_KNOCKDLVR             , ReplyCodeInfo CommandReply "knockd-lvr")
  , (ERR_TOOMANYKNOCK          , ReplyCodeInfo ErrorReply "too-many-knock")
  , (ERR_CHANOPEN              , ReplyCodeInfo ErrorReply "chan-open")
  , (ERR_KNOCKONCHAN           , ReplyCodeInfo ErrorReply "knock-on-chan")
  , (ERR_KNOCKDISABLED         , ReplyCodeInfo ErrorReply "knock-disabled")
  , (RPL_TARGUMODEG            , ReplyCodeInfo CommandReply "targ-umodeg")
  , (RPL_TARGNOTIFY            , ReplyCodeInfo CommandReply "targ-notify")
  , (RPL_UMODEGMSG             , ReplyCodeInfo CommandReply "umodeg-msg")
  , (RPL_OMOTDSTART            , ReplyCodeInfo CommandReply "omotd-start")
  , (RPL_OMOTD                 , ReplyCodeInfo CommandReply "omotd")
  , (RPL_ENDOFOMOTD            , ReplyCodeInfo CommandReply "end-of-omotd")
  , (ERR_NOPRIVS               , ReplyCodeInfo ErrorReply "no-privs")
  , (RPL_TESTMASK              , ReplyCodeInfo CommandReply "test-mask")
  , (RPL_TESTLINE              , ReplyCodeInfo CommandReply "test-line")
  , (RPL_NOTESTLINE            , ReplyCodeInfo CommandReply "no-test-line")
  , (RPL_TESTMASKGECOS         , ReplyCodeInfo CommandReply "testmask-gecos")
  , (RPL_QUIETLIST             , ReplyCodeInfo CommandReply "quiet-list")
  , (RPL_ENDOFQUIETLIST        , ReplyCodeInfo CommandReply "end-of-quiet-list")
  , (RPL_MONONLINE             , ReplyCodeInfo CommandReply "mon-online")
  , (RPL_MONOFFLINE            , ReplyCodeInfo CommandReply "mon-offline")
  , (RPL_MONLIST               , ReplyCodeInfo CommandReply "mon-list")
  , (RPL_ENDOFMONLIST          , ReplyCodeInfo CommandReply "end-of-mon-list")
  , (ERR_MONLISTFULL           , ReplyCodeInfo ErrorReply "mon-list-full")
  , (RPL_RSACHALLENGE2         , ReplyCodeInfo CommandReply "rsa-challenge2")
  , (RPL_ENDOFRSACHALLENGE2    , ReplyCodeInfo CommandReply "end-of-rsa-challenge2")
  , (ERR_MLOCKRESTRICTED       , ReplyCodeInfo ErrorReply "mlock-restricted")
  , (RPL_SCANMATCHED           , ReplyCodeInfo CommandReply "scan-matched")
  , (RPL_SCANUMODES            , ReplyCodeInfo CommandReply "scan-umodes")
  , (RPL_XINFO                 , ReplyCodeInfo CommandReply "xinfo")
  , (RPL_XINFOSTART            , ReplyCodeInfo CommandReply "xinfo-start")
  , (RPL_XINFOEND              , ReplyCodeInfo CommandReply "xinfo-end")
  , (RPL_LOGGEDIN              , ReplyCodeInfo CommandReply "logged-in")
  , (RPL_LOGGEDOUT             , ReplyCodeInfo CommandReply "logged-out")
  , (RPL_NICKLOCKED            , ReplyCodeInfo CommandReply "nick-locked")
  , (RPL_SASLSUCCESS           , ReplyCodeInfo CommandReply "sasl-success")
  , (RPL_SASLFAIL              , ReplyCodeInfo CommandReply "sasl-fail")
  , (RPL_SASLTOOLONG           , ReplyCodeInfo CommandReply "sasl-toolong")
  , (RPL_SASLABORTED           , ReplyCodeInfo CommandReply "sasl-aborted")
  , (RPL_SASLALREADY           , ReplyCodeInfo CommandReply "sasl-already")
  , (RPL_SASLMECHS             , ReplyCodeInfo CommandReply "sasl-mechs")
  , (ERR_CANNOTDOCOMMAND       , ReplyCodeInfo ErrorReply "cannot-do-command")
  , (ERR_CANNOTCHANGEUMODE     , ReplyCodeInfo ErrorReply "cannot-change-umode")
  , (ERR_CANNOTCHANGECHANMODE  , ReplyCodeInfo ErrorReply "cannot-change-chan-mode")
  , (ERR_CANNOTCHANGESERVERMODE, ReplyCodeInfo ErrorReply "cannot-change-server-mode")
  , (ERR_CANNOTSENDTONICK      , ReplyCodeInfo ErrorReply "cannot-send-to-nick")
  , (ERR_UNKNOWNSERVERMODE     , ReplyCodeInfo ErrorReply "unknown-server-mode")
  , (ERR_SERVERMODELOCK        , ReplyCodeInfo ErrorReply "server-mode-lock")
  , (ERR_BADCHARENCODING       , ReplyCodeInfo ErrorReply "bad-char-encoding")
  , (ERR_TOOMANYLANGUAGES      , ReplyCodeInfo ErrorReply "too-many-languages")
  , (ERR_NOLANGUAGE            , ReplyCodeInfo ErrorReply "no-language")
  , (ERR_TEXTTOOSHORT          , ReplyCodeInfo ErrorReply "text-too-short")
  , (ERR_NUMERIC_ERR           , ReplyCodeInfo ErrorReply "numeric-err")
  ]
