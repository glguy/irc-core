{-# Language PatternSynonyms #-}

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

-- | Type of numeric reply codes
newtype ReplyCode = ReplyCode Word

instance Show ReplyCode where
  showsPrec p (ReplyCode x) = showsPrec p x

-- | Categories for reply codes
data ReplyType
  = ClientServerReply -- ^ 0-99 Messages between client and server
  | CommandReply      -- ^ 200-399 Responses to commands
  | ErrorReply        -- ^ 200-399 Errors
  | CustomReply       -- ^ Uncategorized

-- | Categorize replies according to the ranges provided in RFC 2812
replyType :: ReplyCode -> ReplyType
replyType (ReplyCode x)
  |           x < 100 = ClientServerReply
  | 200 <= x, x < 400 = CommandReply
  | 400 <= x, x < 600 = ErrorReply
  | otherwise         = CustomReply

pattern RPL_WELCOME                 :: ReplyCode
pattern RPL_WELCOME                 = ReplyCode 001

pattern RPL_YOURHOST                :: ReplyCode
pattern RPL_YOURHOST                = ReplyCode 002

pattern RPL_CREATED                 :: ReplyCode
pattern RPL_CREATED                 = ReplyCode 003

pattern RPL_MYINFO                  :: ReplyCode
pattern RPL_MYINFO                  = ReplyCode 004

pattern RPL_ISUPPORT                :: ReplyCode
pattern RPL_ISUPPORT                = ReplyCode 005

pattern RPL_SNOMASK                 :: ReplyCode
pattern RPL_SNOMASK                 = ReplyCode 008

pattern RPL_STATMEMTOT              :: ReplyCode
pattern RPL_STATMEMTOT              = ReplyCode 009

pattern RPL_BOUNCE                  :: ReplyCode
pattern RPL_BOUNCE                  = ReplyCode 010

pattern RPL_STATMEM                 :: ReplyCode
pattern RPL_STATMEM                 = ReplyCode 010

pattern RPL_YOURCOOKIE              :: ReplyCode
pattern RPL_YOURCOOKIE              = ReplyCode 014

pattern RPL_YOURID                  :: ReplyCode
pattern RPL_YOURID                  = ReplyCode 042

pattern RPL_SAVENICK                :: ReplyCode
pattern RPL_SAVENICK                = ReplyCode 043

pattern RPL_ATTEMPTINGJUNC          :: ReplyCode
pattern RPL_ATTEMPTINGJUNC          = ReplyCode 050

pattern RPL_ATTEMPTINGREROUTE       :: ReplyCode
pattern RPL_ATTEMPTINGREROUTE       = ReplyCode 051

pattern RPL_TRACELINK               :: ReplyCode
pattern RPL_TRACELINK               = ReplyCode 200

pattern RPL_TRACECONNECTING         :: ReplyCode
pattern RPL_TRACECONNECTING         = ReplyCode 201

pattern RPL_TRACEHANDSHAKE          :: ReplyCode
pattern RPL_TRACEHANDSHAKE          = ReplyCode 202

pattern RPL_TRACEUNKNOWN            :: ReplyCode
pattern RPL_TRACEUNKNOWN            = ReplyCode 203

pattern RPL_TRACEOPERATOR           :: ReplyCode
pattern RPL_TRACEOPERATOR           = ReplyCode 204

pattern RPL_TRACEUSER               :: ReplyCode
pattern RPL_TRACEUSER               = ReplyCode 205

pattern RPL_TRACESERVER             :: ReplyCode
pattern RPL_TRACESERVER             = ReplyCode 206

pattern RPL_TRACESERVICE            :: ReplyCode
pattern RPL_TRACESERVICE            = ReplyCode 207

pattern RPL_TRACENEWTYPE            :: ReplyCode
pattern RPL_TRACENEWTYPE            = ReplyCode 208

pattern RPL_TRACECLASS              :: ReplyCode
pattern RPL_TRACECLASS              = ReplyCode 209

pattern RPL_TRACERECONNECT          :: ReplyCode
pattern RPL_TRACERECONNECT          = ReplyCode 210

pattern RPL_STATS                   :: ReplyCode
pattern RPL_STATS                   = ReplyCode 210

pattern RPL_STATSLINKINFO           :: ReplyCode
pattern RPL_STATSLINKINFO           = ReplyCode 211

pattern RPL_STATSCOMMANDS           :: ReplyCode
pattern RPL_STATSCOMMANDS           = ReplyCode 212

pattern RPL_STATSCLINE              :: ReplyCode
pattern RPL_STATSCLINE              = ReplyCode 213

pattern RPL_STATSILINE              :: ReplyCode
pattern RPL_STATSILINE              = ReplyCode 215

pattern RPL_STATSKLINE              :: ReplyCode
pattern RPL_STATSKLINE              = ReplyCode 216

pattern RPL_STATSYLINE              :: ReplyCode
pattern RPL_STATSYLINE              = ReplyCode 218

pattern RPL_ENDOFSTATS              :: ReplyCode
pattern RPL_ENDOFSTATS              = ReplyCode 219

pattern RPL_UMODEIS                 :: ReplyCode
pattern RPL_UMODEIS                 = ReplyCode 221

pattern RPL_SQLINE_NICK             :: ReplyCode
pattern RPL_SQLINE_NICK             = ReplyCode 222

pattern RPL_STATSDLINE              :: ReplyCode
pattern RPL_STATSDLINE              = ReplyCode 225

pattern RPL_STATSZLINE              :: ReplyCode
pattern RPL_STATSZLINE              = ReplyCode 225

pattern RPL_STATSCOUNT              :: ReplyCode
pattern RPL_STATSCOUNT              = ReplyCode 226

pattern RPL_SERVICEINFO             :: ReplyCode
pattern RPL_SERVICEINFO             = ReplyCode 231

pattern RPL_ENDOFSERVICES           :: ReplyCode
pattern RPL_ENDOFSERVICES           = ReplyCode 232

pattern RPL_SERVICE                 :: ReplyCode
pattern RPL_SERVICE                 = ReplyCode 233

pattern RPL_SERVLIST                :: ReplyCode
pattern RPL_SERVLIST                = ReplyCode 234

pattern RPL_SERVLISTEND             :: ReplyCode
pattern RPL_SERVLISTEND             = ReplyCode 235

pattern RPL_STATSVERBOSE            :: ReplyCode
pattern RPL_STATSVERBOSE            = ReplyCode 236

pattern RPL_STATSIAUTH              :: ReplyCode
pattern RPL_STATSIAUTH              = ReplyCode 239

pattern RPL_STATSLLINE              :: ReplyCode
pattern RPL_STATSLLINE              = ReplyCode 241

pattern RPL_STATSUPTIME             :: ReplyCode
pattern RPL_STATSUPTIME             = ReplyCode 242

pattern RPL_STATSOLINE              :: ReplyCode
pattern RPL_STATSOLINE              = ReplyCode 243

pattern RPL_STATSHLINE              :: ReplyCode
pattern RPL_STATSHLINE              = ReplyCode 244

pattern RPL_STATSSLINE              :: ReplyCode
pattern RPL_STATSSLINE              = ReplyCode 245

pattern RPL_STATSPING               :: ReplyCode
pattern RPL_STATSPING               = ReplyCode 246

pattern RPL_STATSDEFINE             :: ReplyCode
pattern RPL_STATSDEFINE             = ReplyCode 248

pattern RPL_STATSDEBUG              :: ReplyCode
pattern RPL_STATSDEBUG              = ReplyCode 249

pattern RPL_STATSCONN               :: ReplyCode
pattern RPL_STATSCONN               = ReplyCode 250

pattern RPL_LUSERCLIENT             :: ReplyCode
pattern RPL_LUSERCLIENT             = ReplyCode 251

pattern RPL_LUSEROP                 :: ReplyCode
pattern RPL_LUSEROP                 = ReplyCode 252

pattern RPL_LUSERUNKNOWN            :: ReplyCode
pattern RPL_LUSERUNKNOWN            = ReplyCode 253

pattern RPL_LUSERCHANNELS           :: ReplyCode
pattern RPL_LUSERCHANNELS           = ReplyCode 254

pattern RPL_LUSERME                 :: ReplyCode
pattern RPL_LUSERME                 = ReplyCode 255

pattern RPL_ADMINME                 :: ReplyCode
pattern RPL_ADMINME                 = ReplyCode 256

pattern RPL_ADMINLOC1               :: ReplyCode
pattern RPL_ADMINLOC1               = ReplyCode 257

pattern RPL_ADMINLOC2               :: ReplyCode
pattern RPL_ADMINLOC2               = ReplyCode 258

pattern RPL_ADMINEMAIL              :: ReplyCode
pattern RPL_ADMINEMAIL              = ReplyCode 259

pattern RPL_TRACELOG                :: ReplyCode
pattern RPL_TRACELOG                = ReplyCode 261

pattern RPL_TRACEPING               :: ReplyCode
pattern RPL_TRACEPING               = ReplyCode 262

pattern RPL_TRACEEND                :: ReplyCode
pattern RPL_TRACEEND                = ReplyCode 262

pattern RPL_TRYAGAIN                :: ReplyCode
pattern RPL_TRYAGAIN                = ReplyCode 263

pattern RPL_LOCALUSERS              :: ReplyCode
pattern RPL_LOCALUSERS              = ReplyCode 265

pattern RPL_GLOBALUSERS             :: ReplyCode
pattern RPL_GLOBALUSERS             = ReplyCode 266

pattern RPL_START_NETSTAT           :: ReplyCode
pattern RPL_START_NETSTAT           = ReplyCode 267

pattern RPL_NETSTAT                 :: ReplyCode
pattern RPL_NETSTAT                 = ReplyCode 268

pattern RPL_END_NETSTAT             :: ReplyCode
pattern RPL_END_NETSTAT             = ReplyCode 269

pattern RPL_PRIVS                   :: ReplyCode
pattern RPL_PRIVS                   = ReplyCode 270

pattern RPL_SILELIST                :: ReplyCode
pattern RPL_SILELIST                = ReplyCode 271

pattern RPL_ENDOFSILELIST           :: ReplyCode
pattern RPL_ENDOFSILELIST           = ReplyCode 272

pattern RPL_NOTIFY                  :: ReplyCode
pattern RPL_NOTIFY                  = ReplyCode 273

pattern RPL_ENDNOTIFY               :: ReplyCode
pattern RPL_ENDNOTIFY               = ReplyCode 274

pattern RPL_STATSDELTA              :: ReplyCode
pattern RPL_STATSDELTA              = ReplyCode 274

pattern RPL_VCHANEXIST              :: ReplyCode
pattern RPL_VCHANEXIST              = ReplyCode 276

pattern RPL_VCHANLIST               :: ReplyCode
pattern RPL_VCHANLIST               = ReplyCode 277

pattern RPL_VCHANHELP               :: ReplyCode
pattern RPL_VCHANHELP               = ReplyCode 278

pattern RPL_GLIST                   :: ReplyCode
pattern RPL_GLIST                   = ReplyCode 280

pattern RPL_ENDOFGLIST              :: ReplyCode
pattern RPL_ENDOFGLIST              = ReplyCode 281

pattern RPL_ACCEPTLIST              :: ReplyCode
pattern RPL_ACCEPTLIST              = ReplyCode 281

pattern RPL_ENDOFACCEPT             :: ReplyCode
pattern RPL_ENDOFACCEPT             = ReplyCode 282

pattern RPL_JUPELIST                :: ReplyCode
pattern RPL_JUPELIST                = ReplyCode 282

pattern RPL_ENDOFJUPELIST           :: ReplyCode
pattern RPL_ENDOFJUPELIST           = ReplyCode 283

pattern RPL_FEATURE                 :: ReplyCode
pattern RPL_FEATURE                 = ReplyCode 284

pattern RPL_GLIST_HASH              :: ReplyCode
pattern RPL_GLIST_HASH              = ReplyCode 285

pattern RPL_CHANINFO_HANDLE         :: ReplyCode
pattern RPL_CHANINFO_HANDLE         = ReplyCode 285

pattern RPL_NEWHOSTIS               :: ReplyCode
pattern RPL_NEWHOSTIS               = ReplyCode 285

pattern RPL_CHANINFO_USERS          :: ReplyCode
pattern RPL_CHANINFO_USERS          = ReplyCode 286

pattern RPL_CHKHEAD                 :: ReplyCode
pattern RPL_CHKHEAD                 = ReplyCode 286

pattern RPL_CHANINFO_CHOPS          :: ReplyCode
pattern RPL_CHANINFO_CHOPS          = ReplyCode 287

pattern RPL_CHANUSER                :: ReplyCode
pattern RPL_CHANUSER                = ReplyCode 287

pattern RPL_CHANINFO_VOICES         :: ReplyCode
pattern RPL_CHANINFO_VOICES         = ReplyCode 288

pattern RPL_PATCHHEAD               :: ReplyCode
pattern RPL_PATCHHEAD               = ReplyCode 288

pattern RPL_CHANINFO_AWAY           :: ReplyCode
pattern RPL_CHANINFO_AWAY           = ReplyCode 289

pattern RPL_PATCHCON                :: ReplyCode
pattern RPL_PATCHCON                = ReplyCode 289

pattern RPL_CHANINFO_OPERS          :: ReplyCode
pattern RPL_CHANINFO_OPERS          = ReplyCode 290

pattern RPL_HELPHDR                 :: ReplyCode
pattern RPL_HELPHDR                 = ReplyCode 290

pattern RPL_DATASTR                 :: ReplyCode
pattern RPL_DATASTR                 = ReplyCode 290

pattern RPL_CHANINFO_BANNED         :: ReplyCode
pattern RPL_CHANINFO_BANNED         = ReplyCode 291

pattern RPL_HELPOP                  :: ReplyCode
pattern RPL_HELPOP                  = ReplyCode 291

pattern RPL_ENDOFCHECK              :: ReplyCode
pattern RPL_ENDOFCHECK              = ReplyCode 291

pattern RPL_CHANINFO_BANS           :: ReplyCode
pattern RPL_CHANINFO_BANS           = ReplyCode 292

pattern RPL_HELPTLR                 :: ReplyCode
pattern RPL_HELPTLR                 = ReplyCode 292

pattern RPL_CHANINFO_INVITE         :: ReplyCode
pattern RPL_CHANINFO_INVITE         = ReplyCode 293

pattern RPL_HELPHLP                 :: ReplyCode
pattern RPL_HELPHLP                 = ReplyCode 293

pattern RPL_CHANINFO_INVITES        :: ReplyCode
pattern RPL_CHANINFO_INVITES        = ReplyCode 294

pattern RPL_HELPFWD                 :: ReplyCode
pattern RPL_HELPFWD                 = ReplyCode 294

pattern RPL_CHANINFO_KICK           :: ReplyCode
pattern RPL_CHANINFO_KICK           = ReplyCode 295

pattern RPL_HELPIGN                 :: ReplyCode
pattern RPL_HELPIGN                 = ReplyCode 295

pattern RPL_CHANINFO_KICKS          :: ReplyCode
pattern RPL_CHANINFO_KICKS          = ReplyCode 296

pattern RPL_END_CHANINFO            :: ReplyCode
pattern RPL_END_CHANINFO            = ReplyCode 299

pattern RPL_NONE                    :: ReplyCode
pattern RPL_NONE                    = ReplyCode 300

pattern RPL_AWAY                    :: ReplyCode
pattern RPL_AWAY                    = ReplyCode 301

pattern RPL_USERHOST                :: ReplyCode
pattern RPL_USERHOST                = ReplyCode 302

pattern RPL_ISON                    :: ReplyCode
pattern RPL_ISON                    = ReplyCode 303

pattern RPL_TEXT                    :: ReplyCode
pattern RPL_TEXT                    = ReplyCode 304

pattern RPL_UNAWAY                  :: ReplyCode
pattern RPL_UNAWAY                  = ReplyCode 305

pattern RPL_NOWAWAY                 :: ReplyCode
pattern RPL_NOWAWAY                 = ReplyCode 306

pattern RPL_WHOISREGNICK            :: ReplyCode
pattern RPL_WHOISREGNICK            = ReplyCode 307

pattern RPL_SUSERHOST               :: ReplyCode
pattern RPL_SUSERHOST               = ReplyCode 307

pattern RPL_NOTIFYACTION            :: ReplyCode
pattern RPL_NOTIFYACTION            = ReplyCode 308

pattern RPL_WHOISADMIN              :: ReplyCode
pattern RPL_WHOISADMIN              = ReplyCode 308

pattern RPL_NICKTRACE               :: ReplyCode
pattern RPL_NICKTRACE               = ReplyCode 309

pattern RPL_WHOISSADMIN             :: ReplyCode
pattern RPL_WHOISSADMIN             = ReplyCode 309

pattern RPL_WHOISHELPER             :: ReplyCode
pattern RPL_WHOISHELPER             = ReplyCode 309

pattern RPL_WHOISSVCMSG             :: ReplyCode
pattern RPL_WHOISSVCMSG             = ReplyCode 310

pattern RPL_WHOISHELPOP             :: ReplyCode
pattern RPL_WHOISHELPOP             = ReplyCode 310

pattern RPL_WHOISSERVICE            :: ReplyCode
pattern RPL_WHOISSERVICE            = ReplyCode 310

pattern RPL_WHOISUSER               :: ReplyCode
pattern RPL_WHOISUSER               = ReplyCode 311

pattern RPL_WHOISSERVER             :: ReplyCode
pattern RPL_WHOISSERVER             = ReplyCode 312

pattern RPL_WHOISOPERATOR           :: ReplyCode
pattern RPL_WHOISOPERATOR           = ReplyCode 313

pattern RPL_WHOWASUSER              :: ReplyCode
pattern RPL_WHOWASUSER              = ReplyCode 314

pattern RPL_ENDOFWHO                :: ReplyCode
pattern RPL_ENDOFWHO                = ReplyCode 315

pattern RPL_WHOISCHANOP             :: ReplyCode
pattern RPL_WHOISCHANOP             = ReplyCode 316

pattern RPL_WHOISIDLE               :: ReplyCode
pattern RPL_WHOISIDLE               = ReplyCode 317

pattern RPL_ENDOFWHOIS              :: ReplyCode
pattern RPL_ENDOFWHOIS              = ReplyCode 318

pattern RPL_WHOISCHANNELS           :: ReplyCode
pattern RPL_WHOISCHANNELS           = ReplyCode 319

pattern RPL_WHOISVIRT               :: ReplyCode
pattern RPL_WHOISVIRT               = ReplyCode 320

pattern RPL_WHOIS_HIDDEN            :: ReplyCode
pattern RPL_WHOIS_HIDDEN            = ReplyCode 320

pattern RPL_WHOISSPECIAL            :: ReplyCode
pattern RPL_WHOISSPECIAL            = ReplyCode 320

pattern RPL_LISTSTART               :: ReplyCode
pattern RPL_LISTSTART               = ReplyCode 321

pattern RPL_LIST                    :: ReplyCode
pattern RPL_LIST                    = ReplyCode 322

pattern RPL_LISTEND                 :: ReplyCode
pattern RPL_LISTEND                 = ReplyCode 323

pattern RPL_CHANNELMODEIS           :: ReplyCode
pattern RPL_CHANNELMODEIS           = ReplyCode 324

pattern RPL_UNIQOPIS                :: ReplyCode
pattern RPL_UNIQOPIS                = ReplyCode 325

pattern RPL_CHANNELPASSIS           :: ReplyCode
pattern RPL_CHANNELPASSIS           = ReplyCode 325

pattern RPL_NOCHANPASS              :: ReplyCode
pattern RPL_NOCHANPASS              = ReplyCode 326

pattern RPL_CHPASSUNKNOWN           :: ReplyCode
pattern RPL_CHPASSUNKNOWN           = ReplyCode 327

pattern RPL_CHANNEL_URL             :: ReplyCode
pattern RPL_CHANNEL_URL             = ReplyCode 328

pattern RPL_CREATIONTIME            :: ReplyCode
pattern RPL_CREATIONTIME            = ReplyCode 329

pattern RPL_WHOWAS_TIME             :: ReplyCode
pattern RPL_WHOWAS_TIME             = ReplyCode 330

pattern RPL_WHOISACCOUNT            :: ReplyCode
pattern RPL_WHOISACCOUNT            = ReplyCode 330

pattern RPL_NOTOPIC                 :: ReplyCode
pattern RPL_NOTOPIC                 = ReplyCode 331

pattern RPL_TOPIC                   :: ReplyCode
pattern RPL_TOPIC                   = ReplyCode 332

pattern RPL_TOPICWHOTIME            :: ReplyCode
pattern RPL_TOPICWHOTIME            = ReplyCode 333

pattern RPL_LISTUSAGE               :: ReplyCode
pattern RPL_LISTUSAGE               = ReplyCode 334

pattern RPL_COMMANDSYNTAX           :: ReplyCode
pattern RPL_COMMANDSYNTAX           = ReplyCode 334

pattern RPL_LISTSYNTAX              :: ReplyCode
pattern RPL_LISTSYNTAX              = ReplyCode 334

pattern RPL_CHANPASSOK              :: ReplyCode
pattern RPL_CHANPASSOK              = ReplyCode 338

pattern RPL_WHOISACTUALLY           :: ReplyCode
pattern RPL_WHOISACTUALLY           = ReplyCode 338

pattern RPL_BADCHANPASS             :: ReplyCode
pattern RPL_BADCHANPASS             = ReplyCode 339

pattern RPL_INVITING                :: ReplyCode
pattern RPL_INVITING                = ReplyCode 341

pattern RPL_SUMMONING               :: ReplyCode
pattern RPL_SUMMONING               = ReplyCode 342

pattern RPL_INVITED                 :: ReplyCode
pattern RPL_INVITED                 = ReplyCode 345

pattern RPL_INVITELIST              :: ReplyCode
pattern RPL_INVITELIST              = ReplyCode 346

pattern RPL_ENDOFINVITELIST         :: ReplyCode
pattern RPL_ENDOFINVITELIST         = ReplyCode 347

pattern RPL_EXCEPTLIST              :: ReplyCode
pattern RPL_EXCEPTLIST              = ReplyCode 348

pattern RPL_ENDOFEXCEPTLIST         :: ReplyCode
pattern RPL_ENDOFEXCEPTLIST         = ReplyCode 349

pattern RPL_VERSION                 :: ReplyCode
pattern RPL_VERSION                 = ReplyCode 351

pattern RPL_WHOREPLY                :: ReplyCode
pattern RPL_WHOREPLY                = ReplyCode 352

pattern RPL_NAMREPLY                :: ReplyCode
pattern RPL_NAMREPLY                = ReplyCode 353

pattern RPL_WHOSPCRPL               :: ReplyCode
pattern RPL_WHOSPCRPL               = ReplyCode 354

pattern RPL_NAMREPLY_               :: ReplyCode
pattern RPL_NAMREPLY_               = ReplyCode 355

pattern RPL_KILLDONE                :: ReplyCode
pattern RPL_KILLDONE                = ReplyCode 361

pattern RPL_CLOSING                 :: ReplyCode
pattern RPL_CLOSING                 = ReplyCode 362

pattern RPL_CLOSEEND                :: ReplyCode
pattern RPL_CLOSEEND                = ReplyCode 363

pattern RPL_LINKS                   :: ReplyCode
pattern RPL_LINKS                   = ReplyCode 364

pattern RPL_ENDOFLINKS              :: ReplyCode
pattern RPL_ENDOFLINKS              = ReplyCode 365

pattern RPL_ENDOFNAMES              :: ReplyCode
pattern RPL_ENDOFNAMES              = ReplyCode 366

pattern RPL_BANLIST                 :: ReplyCode
pattern RPL_BANLIST                 = ReplyCode 367

pattern RPL_ENDOFBANLIST            :: ReplyCode
pattern RPL_ENDOFBANLIST            = ReplyCode 368

pattern RPL_ENDOFWHOWAS             :: ReplyCode
pattern RPL_ENDOFWHOWAS             = ReplyCode 369

pattern RPL_INFO                    :: ReplyCode
pattern RPL_INFO                    = ReplyCode 371

pattern RPL_MOTD                    :: ReplyCode
pattern RPL_MOTD                    = ReplyCode 372

pattern RPL_INFOSTART               :: ReplyCode
pattern RPL_INFOSTART               = ReplyCode 373

pattern RPL_ENDOFINFO               :: ReplyCode
pattern RPL_ENDOFINFO               = ReplyCode 374

pattern RPL_MOTDSTART               :: ReplyCode
pattern RPL_MOTDSTART               = ReplyCode 375

pattern RPL_ENDOFMOTD               :: ReplyCode
pattern RPL_ENDOFMOTD               = ReplyCode 376

pattern RPL_KICKEXPIRED             :: ReplyCode
pattern RPL_KICKEXPIRED             = ReplyCode 377

pattern RPL_SPAM                    :: ReplyCode
pattern RPL_SPAM                    = ReplyCode 377

pattern RPL_BANEXPIRED              :: ReplyCode
pattern RPL_BANEXPIRED              = ReplyCode 378

pattern RPL_WHOISHOST               :: ReplyCode
pattern RPL_WHOISHOST               = ReplyCode 378

pattern RPL_KICKLINKED              :: ReplyCode
pattern RPL_KICKLINKED              = ReplyCode 379

pattern RPL_BANLINKED               :: ReplyCode
pattern RPL_BANLINKED               = ReplyCode 380

pattern RPL_YOURHELPER              :: ReplyCode
pattern RPL_YOURHELPER              = ReplyCode 380

pattern RPL_YOUREOPER               :: ReplyCode
pattern RPL_YOUREOPER               = ReplyCode 381

pattern RPL_REHASHING               :: ReplyCode
pattern RPL_REHASHING               = ReplyCode 382

pattern RPL_YOURESERVICE            :: ReplyCode
pattern RPL_YOURESERVICE            = ReplyCode 383

pattern RPL_MYPORTIS                :: ReplyCode
pattern RPL_MYPORTIS                = ReplyCode 384

pattern RPL_NOTOPERANYMORE          :: ReplyCode
pattern RPL_NOTOPERANYMORE          = ReplyCode 385

pattern RPL_QLIST                   :: ReplyCode
pattern RPL_QLIST                   = ReplyCode 386

pattern RPL_IRCOPS                  :: ReplyCode
pattern RPL_IRCOPS                  = ReplyCode 386

pattern RPL_ENDOFQLIST              :: ReplyCode
pattern RPL_ENDOFQLIST              = ReplyCode 387

pattern RPL_ENDOFIRCOPS             :: ReplyCode
pattern RPL_ENDOFIRCOPS             = ReplyCode 387

pattern RPL_TIME                    :: ReplyCode
pattern RPL_TIME                    = ReplyCode 391

pattern RPL_USERSSTART              :: ReplyCode
pattern RPL_USERSSTART              = ReplyCode 392

pattern RPL_USERS                   :: ReplyCode
pattern RPL_USERS                   = ReplyCode 393

pattern RPL_ENDOFUSERS              :: ReplyCode
pattern RPL_ENDOFUSERS              = ReplyCode 394

pattern RPL_NOUSERS                 :: ReplyCode
pattern RPL_NOUSERS                 = ReplyCode 395

pattern RPL_HOSTHIDDEN              :: ReplyCode
pattern RPL_HOSTHIDDEN              = ReplyCode 396

pattern ERR_UNKNOWNERROR            :: ReplyCode
pattern ERR_UNKNOWNERROR            = ReplyCode 400

pattern ERR_NOSUCHNICK              :: ReplyCode
pattern ERR_NOSUCHNICK              = ReplyCode 401

pattern ERR_NOSUCHSERVER            :: ReplyCode
pattern ERR_NOSUCHSERVER            = ReplyCode 402

pattern ERR_NOSUCHCHANNEL           :: ReplyCode
pattern ERR_NOSUCHCHANNEL           = ReplyCode 403

pattern ERR_CANNOTSENDTOCHAN        :: ReplyCode
pattern ERR_CANNOTSENDTOCHAN        = ReplyCode 404

pattern ERR_TOOMANYCHANNELS         :: ReplyCode
pattern ERR_TOOMANYCHANNELS         = ReplyCode 405

pattern ERR_WASNOSUCHNICK           :: ReplyCode
pattern ERR_WASNOSUCHNICK           = ReplyCode 406

pattern ERR_TOOMANYTARGETS          :: ReplyCode
pattern ERR_TOOMANYTARGETS          = ReplyCode 407

pattern ERR_NOSUCHSERVICE           :: ReplyCode
pattern ERR_NOSUCHSERVICE           = ReplyCode 408

pattern ERR_NOCOLORSONCHAN          :: ReplyCode
pattern ERR_NOCOLORSONCHAN          = ReplyCode 408

pattern ERR_NOORIGIN                :: ReplyCode
pattern ERR_NOORIGIN                = ReplyCode 409

pattern ERR_NORECIPIENT             :: ReplyCode
pattern ERR_NORECIPIENT             = ReplyCode 411

pattern ERR_NOTEXTTOSEND            :: ReplyCode
pattern ERR_NOTEXTTOSEND            = ReplyCode 412

pattern ERR_NOTOPLEVEL              :: ReplyCode
pattern ERR_NOTOPLEVEL              = ReplyCode 413

pattern ERR_WILDTOPLEVEL            :: ReplyCode
pattern ERR_WILDTOPLEVEL            = ReplyCode 414

pattern ERR_BADMASK                 :: ReplyCode
pattern ERR_BADMASK                 = ReplyCode 415

pattern ERR_TOOMANYMATCHES          :: ReplyCode
pattern ERR_TOOMANYMATCHES          = ReplyCode 416

pattern ERR_QUERYTOOLONG            :: ReplyCode
pattern ERR_QUERYTOOLONG            = ReplyCode 416

pattern ERR_LENGTHTRUNCATED         :: ReplyCode
pattern ERR_LENGTHTRUNCATED         = ReplyCode 419

pattern ERR_UNKNOWNCOMMAND          :: ReplyCode
pattern ERR_UNKNOWNCOMMAND          = ReplyCode 421

pattern ERR_NOMOTD                  :: ReplyCode
pattern ERR_NOMOTD                  = ReplyCode 422

pattern ERR_NOADMININFO             :: ReplyCode
pattern ERR_NOADMININFO             = ReplyCode 423

pattern ERR_FILEERROR               :: ReplyCode
pattern ERR_FILEERROR               = ReplyCode 424

pattern ERR_NOOPERMOTD              :: ReplyCode
pattern ERR_NOOPERMOTD              = ReplyCode 425

pattern ERR_TOOMANYAWAY             :: ReplyCode
pattern ERR_TOOMANYAWAY             = ReplyCode 429

pattern ERR_EVENTNICKCHANGE         :: ReplyCode
pattern ERR_EVENTNICKCHANGE         = ReplyCode 430

pattern ERR_NONICKNAMEGIVEN         :: ReplyCode
pattern ERR_NONICKNAMEGIVEN         = ReplyCode 431

pattern ERR_ERRONEUSNICKNAME        :: ReplyCode
pattern ERR_ERRONEUSNICKNAME        = ReplyCode 432

pattern ERR_NICKNAMEINUSE           :: ReplyCode
pattern ERR_NICKNAMEINUSE           = ReplyCode 433

pattern ERR_SERVICENAMEINUSE        :: ReplyCode
pattern ERR_SERVICENAMEINUSE        = ReplyCode 434

pattern ERR_NORULES                 :: ReplyCode
pattern ERR_NORULES                 = ReplyCode 434

pattern ERR_SERVICECONFUSED         :: ReplyCode
pattern ERR_SERVICECONFUSED         = ReplyCode 435

pattern ERR_BANONCHAN               :: ReplyCode
pattern ERR_BANONCHAN               = ReplyCode 435

pattern ERR_NICKCOLLISION           :: ReplyCode
pattern ERR_NICKCOLLISION           = ReplyCode 436

pattern ERR_UNAVAILRESOURCE         :: ReplyCode
pattern ERR_UNAVAILRESOURCE         = ReplyCode 437

pattern ERR_BANNICKCHANGE           :: ReplyCode
pattern ERR_BANNICKCHANGE           = ReplyCode 437

pattern ERR_NICKTOOFAST             :: ReplyCode
pattern ERR_NICKTOOFAST             = ReplyCode 438

pattern ERR_DEAD                    :: ReplyCode
pattern ERR_DEAD                    = ReplyCode 438

pattern ERR_TARGETTOOFAST           :: ReplyCode
pattern ERR_TARGETTOOFAST           = ReplyCode 439

pattern ERR_SERVICESDOWN            :: ReplyCode
pattern ERR_SERVICESDOWN            = ReplyCode 440

pattern ERR_USERNOTINCHANNEL        :: ReplyCode
pattern ERR_USERNOTINCHANNEL        = ReplyCode 441

pattern ERR_NOTONCHANNEL            :: ReplyCode
pattern ERR_NOTONCHANNEL            = ReplyCode 442

pattern ERR_USERONCHANNEL           :: ReplyCode
pattern ERR_USERONCHANNEL           = ReplyCode 443

pattern ERR_NOLOGIN                 :: ReplyCode
pattern ERR_NOLOGIN                 = ReplyCode 444

pattern ERR_SUMMONDISABLED          :: ReplyCode
pattern ERR_SUMMONDISABLED          = ReplyCode 445

pattern ERR_USERSDISABLED           :: ReplyCode
pattern ERR_USERSDISABLED           = ReplyCode 446

pattern ERR_NONICKCHANGE            :: ReplyCode
pattern ERR_NONICKCHANGE            = ReplyCode 447

pattern ERR_NOTIMPLEMENTED          :: ReplyCode
pattern ERR_NOTIMPLEMENTED          = ReplyCode 449

pattern ERR_NOTREGISTERED           :: ReplyCode
pattern ERR_NOTREGISTERED           = ReplyCode 451

pattern ERR_IDCOLLISION             :: ReplyCode
pattern ERR_IDCOLLISION             = ReplyCode 452

pattern ERR_NICKLOST                :: ReplyCode
pattern ERR_NICKLOST                = ReplyCode 453

pattern ERR_HOSTILENAME             :: ReplyCode
pattern ERR_HOSTILENAME             = ReplyCode 455

pattern ERR_ACCEPTFULL              :: ReplyCode
pattern ERR_ACCEPTFULL              = ReplyCode 456

pattern ERR_ACCEPTEXIST             :: ReplyCode
pattern ERR_ACCEPTEXIST             = ReplyCode 457

pattern ERR_ACCEPTNOT               :: ReplyCode
pattern ERR_ACCEPTNOT               = ReplyCode 458

pattern ERR_NOHIDING                :: ReplyCode
pattern ERR_NOHIDING                = ReplyCode 459

pattern ERR_NOTFORHALFOPS           :: ReplyCode
pattern ERR_NOTFORHALFOPS           = ReplyCode 460

pattern ERR_NEEDMOREPARAMS          :: ReplyCode
pattern ERR_NEEDMOREPARAMS          = ReplyCode 461

pattern ERR_ALREADYREGISTERED       :: ReplyCode
pattern ERR_ALREADYREGISTERED       = ReplyCode 462

pattern ERR_NOPERMFORHOST           :: ReplyCode
pattern ERR_NOPERMFORHOST           = ReplyCode 463

pattern ERR_PASSWDMISMATCH          :: ReplyCode
pattern ERR_PASSWDMISMATCH          = ReplyCode 464

pattern ERR_YOUREBANNEDCREEP        :: ReplyCode
pattern ERR_YOUREBANNEDCREEP        = ReplyCode 465

pattern ERR_YOUWILLBEBANNED         :: ReplyCode
pattern ERR_YOUWILLBEBANNED         = ReplyCode 466

pattern ERR_KEYSET                  :: ReplyCode
pattern ERR_KEYSET                  = ReplyCode 467

pattern ERR_INVALIDUSERNAME         :: ReplyCode
pattern ERR_INVALIDUSERNAME         = ReplyCode 468

pattern ERR_ONLYSERVERSCANCHANGE    :: ReplyCode
pattern ERR_ONLYSERVERSCANCHANGE    = ReplyCode 468

pattern ERR_LINKSET                 :: ReplyCode
pattern ERR_LINKSET                 = ReplyCode 469

pattern ERR_LINKCHANNEL             :: ReplyCode
pattern ERR_LINKCHANNEL             = ReplyCode 470

pattern ERR_KICKEDFROMCHAN          :: ReplyCode
pattern ERR_KICKEDFROMCHAN          = ReplyCode 470

pattern ERR_CHANNELISFULL           :: ReplyCode
pattern ERR_CHANNELISFULL           = ReplyCode 471

pattern ERR_UNKNOWNMODE             :: ReplyCode
pattern ERR_UNKNOWNMODE             = ReplyCode 472

pattern ERR_INVITEONLYCHAN          :: ReplyCode
pattern ERR_INVITEONLYCHAN          = ReplyCode 473

pattern ERR_BANNEDFROMCHAN          :: ReplyCode
pattern ERR_BANNEDFROMCHAN          = ReplyCode 474

pattern ERR_BADCHANNELKEY           :: ReplyCode
pattern ERR_BADCHANNELKEY           = ReplyCode 475

pattern ERR_BADCHANMASK             :: ReplyCode
pattern ERR_BADCHANMASK             = ReplyCode 476

pattern ERR_NOCHANMODES             :: ReplyCode
pattern ERR_NOCHANMODES             = ReplyCode 477

pattern ERR_NEEDREGGEDNICK          :: ReplyCode
pattern ERR_NEEDREGGEDNICK          = ReplyCode 477

pattern ERR_BANLISTFULL             :: ReplyCode
pattern ERR_BANLISTFULL             = ReplyCode 478

pattern ERR_BADCHANNAME             :: ReplyCode
pattern ERR_BADCHANNAME             = ReplyCode 479

pattern ERR_LINKFAIL                :: ReplyCode
pattern ERR_LINKFAIL                = ReplyCode 479

pattern ERR_NOULINE                 :: ReplyCode
pattern ERR_NOULINE                 = ReplyCode 480

pattern ERR_CANNOTKNOCK             :: ReplyCode
pattern ERR_CANNOTKNOCK             = ReplyCode 480

pattern ERR_NOPRIVILEGES            :: ReplyCode
pattern ERR_NOPRIVILEGES            = ReplyCode 481

pattern ERR_CHANOPRIVSNEEDED        :: ReplyCode
pattern ERR_CHANOPRIVSNEEDED        = ReplyCode 482

pattern ERR_CANTKILLSERVER          :: ReplyCode
pattern ERR_CANTKILLSERVER          = ReplyCode 483

pattern ERR_RESTRICTED              :: ReplyCode
pattern ERR_RESTRICTED              = ReplyCode 484

pattern ERR_ISCHANSERVICE           :: ReplyCode
pattern ERR_ISCHANSERVICE           = ReplyCode 484

pattern ERR_DESYNC                  :: ReplyCode
pattern ERR_DESYNC                  = ReplyCode 484

pattern ERR_ATTACKDENY              :: ReplyCode
pattern ERR_ATTACKDENY              = ReplyCode 484

pattern ERR_UNIQOPRIVSNEEDED        :: ReplyCode
pattern ERR_UNIQOPRIVSNEEDED        = ReplyCode 485

pattern ERR_KILLDENY                :: ReplyCode
pattern ERR_KILLDENY                = ReplyCode 485

pattern ERR_CANTKICKADMIN           :: ReplyCode
pattern ERR_CANTKICKADMIN           = ReplyCode 485

pattern ERR_ISREALSERVICE           :: ReplyCode
pattern ERR_ISREALSERVICE           = ReplyCode 485

pattern ERR_NONONREG                :: ReplyCode
pattern ERR_NONONREG                = ReplyCode 486

pattern ERR_HTMDISABLED             :: ReplyCode
pattern ERR_HTMDISABLED             = ReplyCode 486

pattern ERR_ACCOUNTONLY             :: ReplyCode
pattern ERR_ACCOUNTONLY             = ReplyCode 486

pattern ERR_CHANTOORECENT           :: ReplyCode
pattern ERR_CHANTOORECENT           = ReplyCode 487

pattern ERR_MSGSERVICES             :: ReplyCode
pattern ERR_MSGSERVICES             = ReplyCode 487

pattern ERR_TSLESSCHAN              :: ReplyCode
pattern ERR_TSLESSCHAN              = ReplyCode 488

pattern ERR_VOICENEEDED             :: ReplyCode
pattern ERR_VOICENEEDED             = ReplyCode 489

pattern ERR_SECUREONLYCHAN          :: ReplyCode
pattern ERR_SECUREONLYCHAN          = ReplyCode 489

pattern ERR_NOOPERHOST              :: ReplyCode
pattern ERR_NOOPERHOST              = ReplyCode 491

pattern ERR_NOSERVICEHOST           :: ReplyCode
pattern ERR_NOSERVICEHOST           = ReplyCode 492

pattern ERR_NOFEATURE               :: ReplyCode
pattern ERR_NOFEATURE               = ReplyCode 493

pattern ERR_BADFEATURE              :: ReplyCode
pattern ERR_BADFEATURE              = ReplyCode 494

pattern ERR_BADLOGTYPE              :: ReplyCode
pattern ERR_BADLOGTYPE              = ReplyCode 495

pattern ERR_BADLOGSYS               :: ReplyCode
pattern ERR_BADLOGSYS               = ReplyCode 496

pattern ERR_BADLOGVALUE             :: ReplyCode
pattern ERR_BADLOGVALUE             = ReplyCode 497

pattern ERR_ISOPERLCHAN             :: ReplyCode
pattern ERR_ISOPERLCHAN             = ReplyCode 498

pattern ERR_CHANOWNPRIVNEEDED       :: ReplyCode
pattern ERR_CHANOWNPRIVNEEDED       = ReplyCode 499

pattern ERR_UMODEUNKNOWNFLAG        :: ReplyCode
pattern ERR_UMODEUNKNOWNFLAG        = ReplyCode 501

pattern ERR_USERSDONTMATCH          :: ReplyCode
pattern ERR_USERSDONTMATCH          = ReplyCode 502

pattern ERR_GHOSTEDCLIENT           :: ReplyCode
pattern ERR_GHOSTEDCLIENT           = ReplyCode 503

pattern ERR_VWORLDWARN              :: ReplyCode
pattern ERR_VWORLDWARN              = ReplyCode 503

pattern ERR_USERNOTONSERV           :: ReplyCode
pattern ERR_USERNOTONSERV           = ReplyCode 504

pattern ERR_SILELISTFULL            :: ReplyCode
pattern ERR_SILELISTFULL            = ReplyCode 511

pattern ERR_TOOMANYWATCH            :: ReplyCode
pattern ERR_TOOMANYWATCH            = ReplyCode 512

pattern ERR_BADPING                 :: ReplyCode
pattern ERR_BADPING                 = ReplyCode 513

pattern ERR_INVALID_ERROR           :: ReplyCode
pattern ERR_INVALID_ERROR           = ReplyCode 514

pattern ERR_TOOMANYDCC              :: ReplyCode
pattern ERR_TOOMANYDCC              = ReplyCode 514

pattern ERR_BADEXPIRE               :: ReplyCode
pattern ERR_BADEXPIRE               = ReplyCode 515

pattern ERR_DONTCHEAT               :: ReplyCode
pattern ERR_DONTCHEAT               = ReplyCode 516

pattern ERR_DISABLED                :: ReplyCode
pattern ERR_DISABLED                = ReplyCode 517

pattern ERR_NOINVITE                :: ReplyCode
pattern ERR_NOINVITE                = ReplyCode 518

pattern ERR_LONGMASK                :: ReplyCode
pattern ERR_LONGMASK                = ReplyCode 518

pattern ERR_ADMONLY                 :: ReplyCode
pattern ERR_ADMONLY                 = ReplyCode 519

pattern ERR_TOOMANYUSERS            :: ReplyCode
pattern ERR_TOOMANYUSERS            = ReplyCode 519

pattern ERR_OPERONLY                :: ReplyCode
pattern ERR_OPERONLY                = ReplyCode 520

pattern ERR_MASKTOOWIDE             :: ReplyCode
pattern ERR_MASKTOOWIDE             = ReplyCode 520

pattern ERR_WHOTRUNC                :: ReplyCode
pattern ERR_WHOTRUNC                = ReplyCode 520

pattern ERR_LISTSYNTAX              :: ReplyCode
pattern ERR_LISTSYNTAX              = ReplyCode 521

pattern ERR_WHOSYNTAX               :: ReplyCode
pattern ERR_WHOSYNTAX               = ReplyCode 522

pattern ERR_WHOLIMEXCEED            :: ReplyCode
pattern ERR_WHOLIMEXCEED            = ReplyCode 523

pattern ERR_QUARANTINED             :: ReplyCode
pattern ERR_QUARANTINED             = ReplyCode 524

pattern ERR_OPERSPVERIFY            :: ReplyCode
pattern ERR_OPERSPVERIFY            = ReplyCode 524

pattern ERR_REMOTEPFX               :: ReplyCode
pattern ERR_REMOTEPFX               = ReplyCode 525

pattern ERR_PFXUNROUTABLE           :: ReplyCode
pattern ERR_PFXUNROUTABLE           = ReplyCode 526

pattern ERR_BADHOSTMASK             :: ReplyCode
pattern ERR_BADHOSTMASK             = ReplyCode 550

pattern ERR_HOSTUNAVAIL             :: ReplyCode
pattern ERR_HOSTUNAVAIL             = ReplyCode 551

pattern ERR_USINGSLINE              :: ReplyCode
pattern ERR_USINGSLINE              = ReplyCode 552

pattern ERR_STATSSLINE              :: ReplyCode
pattern ERR_STATSSLINE              = ReplyCode 553

pattern RPL_LOGON                   :: ReplyCode
pattern RPL_LOGON                   = ReplyCode 600

pattern RPL_LOGOFF                  :: ReplyCode
pattern RPL_LOGOFF                  = ReplyCode 601

pattern RPL_WATCHOFF                :: ReplyCode
pattern RPL_WATCHOFF                = ReplyCode 602

pattern RPL_WATCHSTAT               :: ReplyCode
pattern RPL_WATCHSTAT               = ReplyCode 603

pattern RPL_NOWON                   :: ReplyCode
pattern RPL_NOWON                   = ReplyCode 604

pattern RPL_NOWOFF                  :: ReplyCode
pattern RPL_NOWOFF                  = ReplyCode 605

pattern RPL_WATCHLIST               :: ReplyCode
pattern RPL_WATCHLIST               = ReplyCode 606

pattern RPL_ENDOFWATCHLIST          :: ReplyCode
pattern RPL_ENDOFWATCHLIST          = ReplyCode 607

pattern RPL_WATCHCLEAR              :: ReplyCode
pattern RPL_WATCHCLEAR              = ReplyCode 608

pattern RPL_ISOPER                  :: ReplyCode
pattern RPL_ISOPER                  = ReplyCode 610

pattern RPL_ISLOCOP                 :: ReplyCode
pattern RPL_ISLOCOP                 = ReplyCode 611

pattern RPL_ISNOTOPER               :: ReplyCode
pattern RPL_ISNOTOPER               = ReplyCode 612

pattern RPL_ENDOFISOPER             :: ReplyCode
pattern RPL_ENDOFISOPER             = ReplyCode 613

pattern RPL_DCCSTATUS               :: ReplyCode
pattern RPL_DCCSTATUS               = ReplyCode 617

pattern RPL_DCCLIST                 :: ReplyCode
pattern RPL_DCCLIST                 = ReplyCode 618

pattern RPL_ENDOFDCCLIST            :: ReplyCode
pattern RPL_ENDOFDCCLIST            = ReplyCode 619

pattern RPL_WHOWASHOST              :: ReplyCode
pattern RPL_WHOWASHOST              = ReplyCode 619

pattern RPL_DCCINFO                 :: ReplyCode
pattern RPL_DCCINFO                 = ReplyCode 620

pattern RPL_RULES                   :: ReplyCode
pattern RPL_RULES                   = ReplyCode 621

pattern RPL_ENDOFO                  :: ReplyCode
pattern RPL_ENDOFO                  = ReplyCode 626

pattern RPL_SETTINGS                :: ReplyCode
pattern RPL_SETTINGS                = ReplyCode 630

pattern RPL_ENDOFSETTINGS           :: ReplyCode
pattern RPL_ENDOFSETTINGS           = ReplyCode 631

pattern RPL_DUMPING                 :: ReplyCode
pattern RPL_DUMPING                 = ReplyCode 640

pattern RPL_DUMPRPL                 :: ReplyCode
pattern RPL_DUMPRPL                 = ReplyCode 641

pattern RPL_EODUMP                  :: ReplyCode
pattern RPL_EODUMP                  = ReplyCode 642

pattern RPL_TRACEROUTE_HOP          :: ReplyCode
pattern RPL_TRACEROUTE_HOP          = ReplyCode 660

pattern RPL_TRACEROUTE_START        :: ReplyCode
pattern RPL_TRACEROUTE_START        = ReplyCode 661

pattern RPL_MODECHANGEWARN          :: ReplyCode
pattern RPL_MODECHANGEWARN          = ReplyCode 662

pattern RPL_CHANREDIR               :: ReplyCode
pattern RPL_CHANREDIR               = ReplyCode 663

pattern RPL_SERVMODEIS              :: ReplyCode
pattern RPL_SERVMODEIS              = ReplyCode 664

pattern RPL_OTHERUMODEIS            :: ReplyCode
pattern RPL_OTHERUMODEIS            = ReplyCode 665

pattern RPL_ENDOF_GENERIC           :: ReplyCode
pattern RPL_ENDOF_GENERIC           = ReplyCode 666

pattern RPL_WHOWASDETAILS           :: ReplyCode
pattern RPL_WHOWASDETAILS           = ReplyCode 670

pattern RPL_WHOISSECURE             :: ReplyCode
pattern RPL_WHOISSECURE             = ReplyCode 671

pattern RPL_UNKNOWNMODES            :: ReplyCode
pattern RPL_UNKNOWNMODES            = ReplyCode 672

pattern RPL_CANNOTSETMODES          :: ReplyCode
pattern RPL_CANNOTSETMODES          = ReplyCode 673

pattern RPL_LUSERSTAFF              :: ReplyCode
pattern RPL_LUSERSTAFF              = ReplyCode 678

pattern RPL_TIMEONSERVERIS          :: ReplyCode
pattern RPL_TIMEONSERVERIS          = ReplyCode 679

pattern RPL_NETWORKS                :: ReplyCode
pattern RPL_NETWORKS                = ReplyCode 682

pattern RPL_YOURLANGUAGEIS          :: ReplyCode
pattern RPL_YOURLANGUAGEIS          = ReplyCode 687

pattern RPL_LANGUAGE                :: ReplyCode
pattern RPL_LANGUAGE                = ReplyCode 688

pattern RPL_WHOISSTAFF              :: ReplyCode
pattern RPL_WHOISSTAFF              = ReplyCode 689

pattern RPL_WHOISLANGUAGE           :: ReplyCode
pattern RPL_WHOISLANGUAGE           = ReplyCode 690

pattern RPL_ENDOFMODLIST            :: ReplyCode
pattern RPL_ENDOFMODLIST            = ReplyCode 703

pattern RPL_HELPSTART               :: ReplyCode
pattern RPL_HELPSTART               = ReplyCode 704

pattern RPL_HELPTXT                 :: ReplyCode
pattern RPL_HELPTXT                 = ReplyCode 705

pattern RPL_ENDOFHELP               :: ReplyCode
pattern RPL_ENDOFHELP               = ReplyCode 706

pattern RPL_ETRACEFULL              :: ReplyCode
pattern RPL_ETRACEFULL              = ReplyCode 708

pattern RPL_ETRACE                  :: ReplyCode
pattern RPL_ETRACE                  = ReplyCode 709

pattern RPL_KNOCK                   :: ReplyCode
pattern RPL_KNOCK                   = ReplyCode 710

pattern RPL_KNOCKDLVR               :: ReplyCode
pattern RPL_KNOCKDLVR               = ReplyCode 711

pattern ERR_TOOMANYKNOCK            :: ReplyCode
pattern ERR_TOOMANYKNOCK            = ReplyCode 712

pattern ERR_CHANOPEN                :: ReplyCode
pattern ERR_CHANOPEN                = ReplyCode 713

pattern ERR_KNOCKONCHAN             :: ReplyCode
pattern ERR_KNOCKONCHAN             = ReplyCode 714

pattern ERR_KNOCKDISABLED           :: ReplyCode
pattern ERR_KNOCKDISABLED           = ReplyCode 715

pattern RPL_TARGUMODEG              :: ReplyCode
pattern RPL_TARGUMODEG              = ReplyCode 716

pattern RPL_TARGNOTIFY              :: ReplyCode
pattern RPL_TARGNOTIFY              = ReplyCode 717

pattern RPL_UMODEGMSG               :: ReplyCode
pattern RPL_UMODEGMSG               = ReplyCode 718

pattern RPL_ENDOFOMOTD              :: ReplyCode
pattern RPL_ENDOFOMOTD              = ReplyCode 722

pattern ERR_NOPRIVS                 :: ReplyCode
pattern ERR_NOPRIVS                 = ReplyCode 723

pattern RPL_TESTMARK                :: ReplyCode
pattern RPL_TESTMARK                = ReplyCode 724

pattern RPL_TESTLINE                :: ReplyCode
pattern RPL_TESTLINE                = ReplyCode 725

pattern RPL_NOTESTLINE              :: ReplyCode
pattern RPL_NOTESTLINE              = ReplyCode 726

pattern RPL_QUIETLIST               :: ReplyCode
pattern RPL_QUIETLIST               = ReplyCode 728

pattern RPL_ENDOFQUIETLIST          :: ReplyCode
pattern RPL_ENDOFQUIETLIST          = ReplyCode 729

pattern RPL_XINFO                   :: ReplyCode
pattern RPL_XINFO                   = ReplyCode 771

pattern RPL_XINFOSTART              :: ReplyCode
pattern RPL_XINFOSTART              = ReplyCode 773

pattern RPL_XINFOEND                :: ReplyCode
pattern RPL_XINFOEND                = ReplyCode 774

pattern RPL_LOGGEDIN                :: ReplyCode
pattern RPL_LOGGEDIN                = ReplyCode 900

pattern RPL_LOGGEDOUT               :: ReplyCode
pattern RPL_LOGGEDOUT               = ReplyCode 901

pattern RPL_NICKLOCKED              :: ReplyCode
pattern RPL_NICKLOCKED              = ReplyCode 902

pattern RPL_SASLSUCCESS             :: ReplyCode
pattern RPL_SASLSUCCESS             = ReplyCode 903

pattern RPL_SASLFAIL                :: ReplyCode
pattern RPL_SASLFAIL                = ReplyCode 904

pattern RPL_SASLTOOLONG             :: ReplyCode
pattern RPL_SASLTOOLONG             = ReplyCode 905

pattern RPL_SASLABORTED             :: ReplyCode
pattern RPL_SASLABORTED             = ReplyCode 906

pattern RPL_SASLALREADY             :: ReplyCode
pattern RPL_SASLALREADY             = ReplyCode 907

pattern RPL_SASLMECHS               :: ReplyCode
pattern RPL_SASLMECHS               = ReplyCode 908

pattern ERR_CANNOTDOCOMMAND         :: ReplyCode
pattern ERR_CANNOTDOCOMMAND         = ReplyCode 972

pattern ERR_CANNOTCHANGEUMODE       :: ReplyCode
pattern ERR_CANNOTCHANGEUMODE       = ReplyCode 973

pattern ERR_CANNOTCHANGECHANMODE    :: ReplyCode
pattern ERR_CANNOTCHANGECHANMODE    = ReplyCode 974

pattern ERR_CANNOTCHANGESERVERMODE  :: ReplyCode
pattern ERR_CANNOTCHANGESERVERMODE  = ReplyCode 975

pattern ERR_CANNOTSENDTONICK        :: ReplyCode
pattern ERR_CANNOTSENDTONICK        = ReplyCode 976

pattern ERR_UNKNOWNSERVERMODE       :: ReplyCode
pattern ERR_UNKNOWNSERVERMODE       = ReplyCode 977

pattern ERR_SERVERMODELOCK          :: ReplyCode
pattern ERR_SERVERMODELOCK          = ReplyCode 979

pattern ERR_BADCHARENCODING         :: ReplyCode
pattern ERR_BADCHARENCODING         = ReplyCode 980

pattern ERR_TOOMANYLANGUAGES        :: ReplyCode
pattern ERR_TOOMANYLANGUAGES        = ReplyCode 981

pattern ERR_NOLANGUAGE              :: ReplyCode
pattern ERR_NOLANGUAGE              = ReplyCode 982

pattern ERR_TEXTTOOSHORT            :: ReplyCode
pattern ERR_TEXTTOOSHORT            = ReplyCode 983

pattern ERR_NUMERIC_ERR             :: ReplyCode
pattern ERR_NUMERIC_ERR             = ReplyCode 999
