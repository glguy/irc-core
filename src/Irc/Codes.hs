{-# Language PatternSynonyms #-}
module Irc.Codes where

-- Extracted from https://www.alien.net.au/irc/irc2numerics.html
pattern RPL_WELCOME                 :: Int
pattern RPL_WELCOME                 = 001

pattern RPL_YOURHOST                :: Int
pattern RPL_YOURHOST                = 002

pattern RPL_CREATED                 :: Int
pattern RPL_CREATED                 = 003

pattern RPL_MYINFO                  :: Int
pattern RPL_MYINFO                  = 004

pattern RPL_ISUPPORT                :: Int
pattern RPL_ISUPPORT                = 005

pattern RPL_SNOMASK                 :: Int
pattern RPL_SNOMASK                 = 008

pattern RPL_STATMEMTOT              :: Int
pattern RPL_STATMEMTOT              = 009

pattern RPL_BOUNCE                  :: Int
pattern RPL_BOUNCE                  = 010

pattern RPL_STATMEM                 :: Int
pattern RPL_STATMEM                 = 010

pattern RPL_YOURCOOKIE              :: Int
pattern RPL_YOURCOOKIE              = 014

pattern RPL_YOURID                  :: Int
pattern RPL_YOURID                  = 042

pattern RPL_SAVENICK                :: Int
pattern RPL_SAVENICK                = 043

pattern RPL_ATTEMPTINGJUNC          :: Int
pattern RPL_ATTEMPTINGJUNC          = 050

pattern RPL_ATTEMPTINGREROUTE       :: Int
pattern RPL_ATTEMPTINGREROUTE       = 051

pattern RPL_TRACELINK               :: Int
pattern RPL_TRACELINK               = 200

pattern RPL_TRACECONNECTING         :: Int
pattern RPL_TRACECONNECTING         = 201

pattern RPL_TRACEHANDSHAKE          :: Int
pattern RPL_TRACEHANDSHAKE          = 202

pattern RPL_TRACEUNKNOWN            :: Int
pattern RPL_TRACEUNKNOWN            = 203

pattern RPL_TRACEOPERATOR           :: Int
pattern RPL_TRACEOPERATOR           = 204

pattern RPL_TRACEUSER               :: Int
pattern RPL_TRACEUSER               = 205

pattern RPL_TRACESERVER             :: Int
pattern RPL_TRACESERVER             = 206

pattern RPL_TRACESERVICE            :: Int
pattern RPL_TRACESERVICE            = 207

pattern RPL_TRACENEWTYPE            :: Int
pattern RPL_TRACENEWTYPE            = 208

pattern RPL_TRACECLASS              :: Int
pattern RPL_TRACECLASS              = 209

pattern RPL_TRACERECONNECT          :: Int
pattern RPL_TRACERECONNECT          = 210

pattern RPL_STATS                   :: Int
pattern RPL_STATS                   = 210

pattern RPL_STATSLINKINFO           :: Int
pattern RPL_STATSLINKINFO           = 211

pattern RPL_STATSCOMMANDS           :: Int
pattern RPL_STATSCOMMANDS           = 212

pattern RPL_STATSCLINE              :: Int
pattern RPL_STATSCLINE              = 213

pattern RPL_STATSILINE              :: Int
pattern RPL_STATSILINE              = 215

pattern RPL_STATSKLINE              :: Int
pattern RPL_STATSKLINE              = 216

pattern RPL_STATSYLINE              :: Int
pattern RPL_STATSYLINE              = 218

pattern RPL_ENDOFSTATS              :: Int
pattern RPL_ENDOFSTATS              = 219

pattern RPL_UMODEIS                 :: Int
pattern RPL_UMODEIS                 = 221

pattern RPL_SQLINE_NICK             :: Int
pattern RPL_SQLINE_NICK             = 222

pattern RPL_STATSDLINE              :: Int
pattern RPL_STATSDLINE              = 225

pattern RPL_STATSZLINE              :: Int
pattern RPL_STATSZLINE              = 225

pattern RPL_STATSCOUNT              :: Int
pattern RPL_STATSCOUNT              = 226

pattern RPL_SERVICEINFO             :: Int
pattern RPL_SERVICEINFO             = 231

pattern RPL_ENDOFSERVICES           :: Int
pattern RPL_ENDOFSERVICES           = 232

pattern RPL_SERVICE                 :: Int
pattern RPL_SERVICE                 = 233

pattern RPL_SERVLIST                :: Int
pattern RPL_SERVLIST                = 234

pattern RPL_SERVLISTEND             :: Int
pattern RPL_SERVLISTEND             = 235

pattern RPL_STATSVERBOSE            :: Int
pattern RPL_STATSVERBOSE            = 236

pattern RPL_STATSIAUTH              :: Int
pattern RPL_STATSIAUTH              = 239

pattern RPL_STATSLLINE              :: Int
pattern RPL_STATSLLINE              = 241

pattern RPL_STATSUPTIME             :: Int
pattern RPL_STATSUPTIME             = 242

pattern RPL_STATSOLINE              :: Int
pattern RPL_STATSOLINE              = 243

pattern RPL_STATSHLINE              :: Int
pattern RPL_STATSHLINE              = 244

pattern RPL_STATSSLINE              :: Int
pattern RPL_STATSSLINE              = 245

pattern RPL_STATSPING               :: Int
pattern RPL_STATSPING               = 246

pattern RPL_STATSDEFINE             :: Int
pattern RPL_STATSDEFINE             = 248

pattern RPL_STATSDEBUG              :: Int
pattern RPL_STATSDEBUG              = 249

pattern RPL_STATSCONN               :: Int
pattern RPL_STATSCONN               = 250

pattern RPL_LUSERCLIENT             :: Int
pattern RPL_LUSERCLIENT             = 251

pattern RPL_LUSEROP                 :: Int
pattern RPL_LUSEROP                 = 252

pattern RPL_LUSERUNKNOWN            :: Int
pattern RPL_LUSERUNKNOWN            = 253

pattern RPL_LUSERCHANNELS           :: Int
pattern RPL_LUSERCHANNELS           = 254

pattern RPL_LUSERME                 :: Int
pattern RPL_LUSERME                 = 255

pattern RPL_ADMINME                 :: Int
pattern RPL_ADMINME                 = 256

pattern RPL_ADMINLOC1               :: Int
pattern RPL_ADMINLOC1               = 257

pattern RPL_ADMINLOC2               :: Int
pattern RPL_ADMINLOC2               = 258

pattern RPL_ADMINEMAIL              :: Int
pattern RPL_ADMINEMAIL              = 259

pattern RPL_TRACELOG                :: Int
pattern RPL_TRACELOG                = 261

pattern RPL_TRACEPING               :: Int
pattern RPL_TRACEPING               = 262

pattern RPL_TRACEEND                :: Int
pattern RPL_TRACEEND                = 262

pattern RPL_TRYAGAIN                :: Int
pattern RPL_TRYAGAIN                = 263

pattern RPL_LOCALUSERS              :: Int
pattern RPL_LOCALUSERS              = 265

pattern RPL_GLOBALUSERS             :: Int
pattern RPL_GLOBALUSERS             = 266

pattern RPL_START_NETSTAT           :: Int
pattern RPL_START_NETSTAT           = 267

pattern RPL_NETSTAT                 :: Int
pattern RPL_NETSTAT                 = 268

pattern RPL_END_NETSTAT             :: Int
pattern RPL_END_NETSTAT             = 269

pattern RPL_PRIVS                   :: Int
pattern RPL_PRIVS                   = 270

pattern RPL_SILELIST                :: Int
pattern RPL_SILELIST                = 271

pattern RPL_ENDOFSILELIST           :: Int
pattern RPL_ENDOFSILELIST           = 272

pattern RPL_NOTIFY                  :: Int
pattern RPL_NOTIFY                  = 273

pattern RPL_ENDNOTIFY               :: Int
pattern RPL_ENDNOTIFY               = 274

pattern RPL_STATSDELTA              :: Int
pattern RPL_STATSDELTA              = 274

pattern RPL_VCHANEXIST              :: Int
pattern RPL_VCHANEXIST              = 276

pattern RPL_VCHANLIST               :: Int
pattern RPL_VCHANLIST               = 277

pattern RPL_VCHANHELP               :: Int
pattern RPL_VCHANHELP               = 278

pattern RPL_GLIST                   :: Int
pattern RPL_GLIST                   = 280

pattern RPL_ENDOFGLIST              :: Int
pattern RPL_ENDOFGLIST              = 281

pattern RPL_ACCEPTLIST              :: Int
pattern RPL_ACCEPTLIST              = 281

pattern RPL_ENDOFACCEPT             :: Int
pattern RPL_ENDOFACCEPT             = 282

pattern RPL_JUPELIST                :: Int
pattern RPL_JUPELIST                = 282

pattern RPL_ENDOFJUPELIST           :: Int
pattern RPL_ENDOFJUPELIST           = 283

pattern RPL_FEATURE                 :: Int
pattern RPL_FEATURE                 = 284

pattern RPL_GLIST_HASH              :: Int
pattern RPL_GLIST_HASH              = 285

pattern RPL_CHANINFO_HANDLE         :: Int
pattern RPL_CHANINFO_HANDLE         = 285

pattern RPL_NEWHOSTIS               :: Int
pattern RPL_NEWHOSTIS               = 285

pattern RPL_CHANINFO_USERS          :: Int
pattern RPL_CHANINFO_USERS          = 286

pattern RPL_CHKHEAD                 :: Int
pattern RPL_CHKHEAD                 = 286

pattern RPL_CHANINFO_CHOPS          :: Int
pattern RPL_CHANINFO_CHOPS          = 287

pattern RPL_CHANUSER                :: Int
pattern RPL_CHANUSER                = 287

pattern RPL_CHANINFO_VOICES         :: Int
pattern RPL_CHANINFO_VOICES         = 288

pattern RPL_PATCHHEAD               :: Int
pattern RPL_PATCHHEAD               = 288

pattern RPL_CHANINFO_AWAY           :: Int
pattern RPL_CHANINFO_AWAY           = 289

pattern RPL_PATCHCON                :: Int
pattern RPL_PATCHCON                = 289

pattern RPL_CHANINFO_OPERS          :: Int
pattern RPL_CHANINFO_OPERS          = 290

pattern RPL_HELPHDR                 :: Int
pattern RPL_HELPHDR                 = 290

pattern RPL_DATASTR                 :: Int
pattern RPL_DATASTR                 = 290

pattern RPL_CHANINFO_BANNED         :: Int
pattern RPL_CHANINFO_BANNED         = 291

pattern RPL_HELPOP                  :: Int
pattern RPL_HELPOP                  = 291

pattern RPL_ENDOFCHECK              :: Int
pattern RPL_ENDOFCHECK              = 291

pattern RPL_CHANINFO_BANS           :: Int
pattern RPL_CHANINFO_BANS           = 292

pattern RPL_HELPTLR                 :: Int
pattern RPL_HELPTLR                 = 292

pattern RPL_CHANINFO_INVITE         :: Int
pattern RPL_CHANINFO_INVITE         = 293

pattern RPL_HELPHLP                 :: Int
pattern RPL_HELPHLP                 = 293

pattern RPL_CHANINFO_INVITES        :: Int
pattern RPL_CHANINFO_INVITES        = 294

pattern RPL_HELPFWD                 :: Int
pattern RPL_HELPFWD                 = 294

pattern RPL_CHANINFO_KICK           :: Int
pattern RPL_CHANINFO_KICK           = 295

pattern RPL_HELPIGN                 :: Int
pattern RPL_HELPIGN                 = 295

pattern RPL_CHANINFO_KICKS          :: Int
pattern RPL_CHANINFO_KICKS          = 296

pattern RPL_END_CHANINFO            :: Int
pattern RPL_END_CHANINFO            = 299

pattern RPL_NONE                    :: Int
pattern RPL_NONE                    = 300

pattern RPL_AWAY                    :: Int
pattern RPL_AWAY                    = 301

pattern RPL_USERHOST                :: Int
pattern RPL_USERHOST                = 302

pattern RPL_ISON                    :: Int
pattern RPL_ISON                    = 303

pattern RPL_TEXT                    :: Int
pattern RPL_TEXT                    = 304

pattern RPL_UNAWAY                  :: Int
pattern RPL_UNAWAY                  = 305

pattern RPL_NOWAWAY                 :: Int
pattern RPL_NOWAWAY                 = 306

pattern RPL_WHOISREGNICK            :: Int
pattern RPL_WHOISREGNICK            = 307

pattern RPL_SUSERHOST               :: Int
pattern RPL_SUSERHOST               = 307

pattern RPL_NOTIFYACTION            :: Int
pattern RPL_NOTIFYACTION            = 308

pattern RPL_WHOISADMIN              :: Int
pattern RPL_WHOISADMIN              = 308

pattern RPL_NICKTRACE               :: Int
pattern RPL_NICKTRACE               = 309

pattern RPL_WHOISSADMIN             :: Int
pattern RPL_WHOISSADMIN             = 309

pattern RPL_WHOISHELPER             :: Int
pattern RPL_WHOISHELPER             = 309

pattern RPL_WHOISSVCMSG             :: Int
pattern RPL_WHOISSVCMSG             = 310

pattern RPL_WHOISHELPOP             :: Int
pattern RPL_WHOISHELPOP             = 310

pattern RPL_WHOISSERVICE            :: Int
pattern RPL_WHOISSERVICE            = 310

pattern RPL_WHOISUSER               :: Int
pattern RPL_WHOISUSER               = 311

pattern RPL_WHOISSERVER             :: Int
pattern RPL_WHOISSERVER             = 312

pattern RPL_WHOISOPERATOR           :: Int
pattern RPL_WHOISOPERATOR           = 313

pattern RPL_WHOWASUSER              :: Int
pattern RPL_WHOWASUSER              = 314

pattern RPL_ENDOFWHO                :: Int
pattern RPL_ENDOFWHO                = 315

pattern RPL_WHOISCHANOP             :: Int
pattern RPL_WHOISCHANOP             = 316

pattern RPL_WHOISIDLE               :: Int
pattern RPL_WHOISIDLE               = 317

pattern RPL_ENDOFWHOIS              :: Int
pattern RPL_ENDOFWHOIS              = 318

pattern RPL_WHOISCHANNELS           :: Int
pattern RPL_WHOISCHANNELS           = 319

pattern RPL_WHOISVIRT               :: Int
pattern RPL_WHOISVIRT               = 320

pattern RPL_WHOIS_HIDDEN            :: Int
pattern RPL_WHOIS_HIDDEN            = 320

pattern RPL_WHOISSPECIAL            :: Int
pattern RPL_WHOISSPECIAL            = 320

pattern RPL_LISTSTART               :: Int
pattern RPL_LISTSTART               = 321

pattern RPL_LIST                    :: Int
pattern RPL_LIST                    = 322

pattern RPL_LISTEND                 :: Int
pattern RPL_LISTEND                 = 323

pattern RPL_CHANNELMODEIS           :: Int
pattern RPL_CHANNELMODEIS           = 324

pattern RPL_UNIQOPIS                :: Int
pattern RPL_UNIQOPIS                = 325

pattern RPL_CHANNELPASSIS           :: Int
pattern RPL_CHANNELPASSIS           = 325

pattern RPL_NOCHANPASS              :: Int
pattern RPL_NOCHANPASS              = 326

pattern RPL_CHPASSUNKNOWN           :: Int
pattern RPL_CHPASSUNKNOWN           = 327

pattern RPL_CHANNEL_URL             :: Int
pattern RPL_CHANNEL_URL             = 328

pattern RPL_CREATIONTIME            :: Int
pattern RPL_CREATIONTIME            = 329

pattern RPL_WHOWAS_TIME             :: Int
pattern RPL_WHOWAS_TIME             = 330

pattern RPL_WHOISACCOUNT            :: Int
pattern RPL_WHOISACCOUNT            = 330

pattern RPL_NOTOPIC                 :: Int
pattern RPL_NOTOPIC                 = 331

pattern RPL_TOPIC                   :: Int
pattern RPL_TOPIC                   = 332

pattern RPL_TOPICWHOTIME            :: Int
pattern RPL_TOPICWHOTIME            = 333

pattern RPL_LISTUSAGE               :: Int
pattern RPL_LISTUSAGE               = 334

pattern RPL_COMMANDSYNTAX           :: Int
pattern RPL_COMMANDSYNTAX           = 334

pattern RPL_LISTSYNTAX              :: Int
pattern RPL_LISTSYNTAX              = 334

pattern RPL_CHANPASSOK              :: Int
pattern RPL_CHANPASSOK              = 338

pattern RPL_WHOISACTUALLY           :: Int
pattern RPL_WHOISACTUALLY           = 338

pattern RPL_BADCHANPASS             :: Int
pattern RPL_BADCHANPASS             = 339

pattern RPL_INVITING                :: Int
pattern RPL_INVITING                = 341

pattern RPL_SUMMONING               :: Int
pattern RPL_SUMMONING               = 342

pattern RPL_INVITED                 :: Int
pattern RPL_INVITED                 = 345

pattern RPL_INVITELIST              :: Int
pattern RPL_INVITELIST              = 346

pattern RPL_ENDOFINVITELIST         :: Int
pattern RPL_ENDOFINVITELIST         = 347

pattern RPL_EXCEPTLIST              :: Int
pattern RPL_EXCEPTLIST              = 348

pattern RPL_ENDOFEXCEPTLIST         :: Int
pattern RPL_ENDOFEXCEPTLIST         = 349

pattern RPL_VERSION                 :: Int
pattern RPL_VERSION                 = 351

pattern RPL_WHOREPLY                :: Int
pattern RPL_WHOREPLY                = 352

pattern RPL_NAMREPLY                :: Int
pattern RPL_NAMREPLY                = 353

pattern RPL_WHOSPCRPL               :: Int
pattern RPL_WHOSPCRPL               = 354

pattern RPL_NAMREPLY_               :: Int
pattern RPL_NAMREPLY_               = 355

pattern RPL_KILLDONE                :: Int
pattern RPL_KILLDONE                = 361

pattern RPL_CLOSING                 :: Int
pattern RPL_CLOSING                 = 362

pattern RPL_CLOSEEND                :: Int
pattern RPL_CLOSEEND                = 363

pattern RPL_LINKS                   :: Int
pattern RPL_LINKS                   = 364

pattern RPL_ENDOFLINKS              :: Int
pattern RPL_ENDOFLINKS              = 365

pattern RPL_ENDOFNAMES              :: Int
pattern RPL_ENDOFNAMES              = 366

pattern RPL_BANLIST                 :: Int
pattern RPL_BANLIST                 = 367

pattern RPL_ENDOFBANLIST            :: Int
pattern RPL_ENDOFBANLIST            = 368

pattern RPL_ENDOFWHOWAS             :: Int
pattern RPL_ENDOFWHOWAS             = 369

pattern RPL_INFO                    :: Int
pattern RPL_INFO                    = 371

pattern RPL_MOTD                    :: Int
pattern RPL_MOTD                    = 372

pattern RPL_INFOSTART               :: Int
pattern RPL_INFOSTART               = 373

pattern RPL_ENDOFINFO               :: Int
pattern RPL_ENDOFINFO               = 374

pattern RPL_MOTDSTART               :: Int
pattern RPL_MOTDSTART               = 375

pattern RPL_ENDOFMOTD               :: Int
pattern RPL_ENDOFMOTD               = 376

pattern RPL_KICKEXPIRED             :: Int
pattern RPL_KICKEXPIRED             = 377

pattern RPL_SPAM                    :: Int
pattern RPL_SPAM                    = 377

pattern RPL_BANEXPIRED              :: Int
pattern RPL_BANEXPIRED              = 378

pattern RPL_WHOISHOST               :: Int
pattern RPL_WHOISHOST               = 378

pattern RPL_KICKLINKED              :: Int
pattern RPL_KICKLINKED              = 379

pattern RPL_BANLINKED               :: Int
pattern RPL_BANLINKED               = 380

pattern RPL_YOURHELPER              :: Int
pattern RPL_YOURHELPER              = 380

pattern RPL_YOUREOPER               :: Int
pattern RPL_YOUREOPER               = 381

pattern RPL_REHASHING               :: Int
pattern RPL_REHASHING               = 382

pattern RPL_YOURESERVICE            :: Int
pattern RPL_YOURESERVICE            = 383

pattern RPL_MYPORTIS                :: Int
pattern RPL_MYPORTIS                = 384

pattern RPL_NOTOPERANYMORE          :: Int
pattern RPL_NOTOPERANYMORE          = 385

pattern RPL_QLIST                   :: Int
pattern RPL_QLIST                   = 386

pattern RPL_IRCOPS                  :: Int
pattern RPL_IRCOPS                  = 386

pattern RPL_ENDOFQLIST              :: Int
pattern RPL_ENDOFQLIST              = 387

pattern RPL_ENDOFIRCOPS             :: Int
pattern RPL_ENDOFIRCOPS             = 387

pattern RPL_TIME                    :: Int
pattern RPL_TIME                    = 391

pattern RPL_USERSSTART              :: Int
pattern RPL_USERSSTART              = 392

pattern RPL_USERS                   :: Int
pattern RPL_USERS                   = 393

pattern RPL_ENDOFUSERS              :: Int
pattern RPL_ENDOFUSERS              = 394

pattern RPL_NOUSERS                 :: Int
pattern RPL_NOUSERS                 = 395

pattern RPL_HOSTHIDDEN              :: Int
pattern RPL_HOSTHIDDEN              = 396

pattern ERR_UNKNOWNERROR            :: Int
pattern ERR_UNKNOWNERROR            = 400

pattern ERR_NOSUCHNICK              :: Int
pattern ERR_NOSUCHNICK              = 401

pattern ERR_NOSUCHSERVER            :: Int
pattern ERR_NOSUCHSERVER            = 402

pattern ERR_NOSUCHCHANNEL           :: Int
pattern ERR_NOSUCHCHANNEL           = 403

pattern ERR_CANNOTSENDTOCHAN        :: Int
pattern ERR_CANNOTSENDTOCHAN        = 404

pattern ERR_TOOMANYCHANNELS         :: Int
pattern ERR_TOOMANYCHANNELS         = 405

pattern ERR_WASNOSUCHNICK           :: Int
pattern ERR_WASNOSUCHNICK           = 406

pattern ERR_TOOMANYTARGETS          :: Int
pattern ERR_TOOMANYTARGETS          = 407

pattern ERR_NOSUCHSERVICE           :: Int
pattern ERR_NOSUCHSERVICE           = 408

pattern ERR_NOCOLORSONCHAN          :: Int
pattern ERR_NOCOLORSONCHAN          = 408

pattern ERR_NOORIGIN                :: Int
pattern ERR_NOORIGIN                = 409

pattern ERR_NORECIPIENT             :: Int
pattern ERR_NORECIPIENT             = 411

pattern ERR_NOTEXTTOSEND            :: Int
pattern ERR_NOTEXTTOSEND            = 412

pattern ERR_NOTOPLEVEL              :: Int
pattern ERR_NOTOPLEVEL              = 413

pattern ERR_WILDTOPLEVEL            :: Int
pattern ERR_WILDTOPLEVEL            = 414

pattern ERR_BADMASK                 :: Int
pattern ERR_BADMASK                 = 415

pattern ERR_TOOMANYMATCHES          :: Int
pattern ERR_TOOMANYMATCHES          = 416

pattern ERR_QUERYTOOLONG            :: Int
pattern ERR_QUERYTOOLONG            = 416

pattern ERR_LENGTHTRUNCATED         :: Int
pattern ERR_LENGTHTRUNCATED         = 419

pattern ERR_UNKNOWNCOMMAND          :: Int
pattern ERR_UNKNOWNCOMMAND          = 421

pattern ERR_NOMOTD                  :: Int
pattern ERR_NOMOTD                  = 422

pattern ERR_NOADMININFO             :: Int
pattern ERR_NOADMININFO             = 423

pattern ERR_FILEERROR               :: Int
pattern ERR_FILEERROR               = 424

pattern ERR_NOOPERMOTD              :: Int
pattern ERR_NOOPERMOTD              = 425

pattern ERR_TOOMANYAWAY             :: Int
pattern ERR_TOOMANYAWAY             = 429

pattern ERR_EVENTNICKCHANGE         :: Int
pattern ERR_EVENTNICKCHANGE         = 430

pattern ERR_NONICKNAMEGIVEN         :: Int
pattern ERR_NONICKNAMEGIVEN         = 431

pattern ERR_ERRONEUSNICKNAME        :: Int
pattern ERR_ERRONEUSNICKNAME        = 432

pattern ERR_NICKNAMEINUSE           :: Int
pattern ERR_NICKNAMEINUSE           = 433

pattern ERR_SERVICENAMEINUSE        :: Int
pattern ERR_SERVICENAMEINUSE        = 434

pattern ERR_NORULES                 :: Int
pattern ERR_NORULES                 = 434

pattern ERR_SERVICECONFUSED         :: Int
pattern ERR_SERVICECONFUSED         = 435

pattern ERR_BANONCHAN               :: Int
pattern ERR_BANONCHAN               = 435

pattern ERR_NICKCOLLISION           :: Int
pattern ERR_NICKCOLLISION           = 436

pattern ERR_UNAVAILRESOURCE         :: Int
pattern ERR_UNAVAILRESOURCE         = 437

pattern ERR_BANNICKCHANGE           :: Int
pattern ERR_BANNICKCHANGE           = 437

pattern ERR_NICKTOOFAST             :: Int
pattern ERR_NICKTOOFAST             = 438

pattern ERR_DEAD                    :: Int
pattern ERR_DEAD                    = 438

pattern ERR_TARGETTOOFAST           :: Int
pattern ERR_TARGETTOOFAST           = 439

pattern ERR_SERVICESDOWN            :: Int
pattern ERR_SERVICESDOWN            = 440

pattern ERR_USERNOTINCHANNEL        :: Int
pattern ERR_USERNOTINCHANNEL        = 441

pattern ERR_NOTONCHANNEL            :: Int
pattern ERR_NOTONCHANNEL            = 442

pattern ERR_USERONCHANNEL           :: Int
pattern ERR_USERONCHANNEL           = 443

pattern ERR_NOLOGIN                 :: Int
pattern ERR_NOLOGIN                 = 444

pattern ERR_SUMMONDISABLED          :: Int
pattern ERR_SUMMONDISABLED          = 445

pattern ERR_USERSDISABLED           :: Int
pattern ERR_USERSDISABLED           = 446

pattern ERR_NONICKCHANGE            :: Int
pattern ERR_NONICKCHANGE            = 447

pattern ERR_NOTIMPLEMENTED          :: Int
pattern ERR_NOTIMPLEMENTED          = 449

pattern ERR_NOTREGISTERED           :: Int
pattern ERR_NOTREGISTERED           = 451

pattern ERR_IDCOLLISION             :: Int
pattern ERR_IDCOLLISION             = 452

pattern ERR_NICKLOST                :: Int
pattern ERR_NICKLOST                = 453

pattern ERR_HOSTILENAME             :: Int
pattern ERR_HOSTILENAME             = 455

pattern ERR_ACCEPTFULL              :: Int
pattern ERR_ACCEPTFULL              = 456

pattern ERR_ACCEPTEXIST             :: Int
pattern ERR_ACCEPTEXIST             = 457

pattern ERR_ACCEPTNOT               :: Int
pattern ERR_ACCEPTNOT               = 458

pattern ERR_NOHIDING                :: Int
pattern ERR_NOHIDING                = 459

pattern ERR_NOTFORHALFOPS           :: Int
pattern ERR_NOTFORHALFOPS           = 460

pattern ERR_NEEDMOREPARAMS          :: Int
pattern ERR_NEEDMOREPARAMS          = 461

pattern ERR_ALREADYREGISTERED       :: Int
pattern ERR_ALREADYREGISTERED       = 462

pattern ERR_NOPERMFORHOST           :: Int
pattern ERR_NOPERMFORHOST           = 463

pattern ERR_PASSWDMISMATCH          :: Int
pattern ERR_PASSWDMISMATCH          = 464

pattern ERR_YOUREBANNEDCREEP        :: Int
pattern ERR_YOUREBANNEDCREEP        = 465

pattern ERR_YOUWILLBEBANNED         :: Int
pattern ERR_YOUWILLBEBANNED         = 466

pattern ERR_KEYSET                  :: Int
pattern ERR_KEYSET                  = 467

pattern ERR_INVALIDUSERNAME         :: Int
pattern ERR_INVALIDUSERNAME         = 468

pattern ERR_ONLYSERVERSCANCHANGE    :: Int
pattern ERR_ONLYSERVERSCANCHANGE    = 468

pattern ERR_LINKSET                 :: Int
pattern ERR_LINKSET                 = 469

pattern ERR_LINKCHANNEL             :: Int
pattern ERR_LINKCHANNEL             = 470

pattern ERR_KICKEDFROMCHAN          :: Int
pattern ERR_KICKEDFROMCHAN          = 470

pattern ERR_CHANNELISFULL           :: Int
pattern ERR_CHANNELISFULL           = 471

pattern ERR_UNKNOWNMODE             :: Int
pattern ERR_UNKNOWNMODE             = 472

pattern ERR_INVITEONLYCHAN          :: Int
pattern ERR_INVITEONLYCHAN          = 473

pattern ERR_BANNEDFROMCHAN          :: Int
pattern ERR_BANNEDFROMCHAN          = 474

pattern ERR_BADCHANNELKEY           :: Int
pattern ERR_BADCHANNELKEY           = 475

pattern ERR_BADCHANMASK             :: Int
pattern ERR_BADCHANMASK             = 476

pattern ERR_NOCHANMODES             :: Int
pattern ERR_NOCHANMODES             = 477

pattern ERR_NEEDREGGEDNICK          :: Int
pattern ERR_NEEDREGGEDNICK          = 477

pattern ERR_BANLISTFULL             :: Int
pattern ERR_BANLISTFULL             = 478

pattern ERR_BADCHANNAME             :: Int
pattern ERR_BADCHANNAME             = 479

pattern ERR_LINKFAIL                :: Int
pattern ERR_LINKFAIL                = 479

pattern ERR_NOULINE                 :: Int
pattern ERR_NOULINE                 = 480

pattern ERR_CANNOTKNOCK             :: Int
pattern ERR_CANNOTKNOCK             = 480

pattern ERR_NOPRIVILEGES            :: Int
pattern ERR_NOPRIVILEGES            = 481

pattern ERR_CHANOPRIVSNEEDED        :: Int
pattern ERR_CHANOPRIVSNEEDED        = 482

pattern ERR_CANTKILLSERVER          :: Int
pattern ERR_CANTKILLSERVER          = 483

pattern ERR_RESTRICTED              :: Int
pattern ERR_RESTRICTED              = 484

pattern ERR_ISCHANSERVICE           :: Int
pattern ERR_ISCHANSERVICE           = 484

pattern ERR_DESYNC                  :: Int
pattern ERR_DESYNC                  = 484

pattern ERR_ATTACKDENY              :: Int
pattern ERR_ATTACKDENY              = 484

pattern ERR_UNIQOPRIVSNEEDED        :: Int
pattern ERR_UNIQOPRIVSNEEDED        = 485

pattern ERR_KILLDENY                :: Int
pattern ERR_KILLDENY                = 485

pattern ERR_CANTKICKADMIN           :: Int
pattern ERR_CANTKICKADMIN           = 485

pattern ERR_ISREALSERVICE           :: Int
pattern ERR_ISREALSERVICE           = 485

pattern ERR_NONONREG                :: Int
pattern ERR_NONONREG                = 486

pattern ERR_HTMDISABLED             :: Int
pattern ERR_HTMDISABLED             = 486

pattern ERR_ACCOUNTONLY             :: Int
pattern ERR_ACCOUNTONLY             = 486

pattern ERR_CHANTOORECENT           :: Int
pattern ERR_CHANTOORECENT           = 487

pattern ERR_MSGSERVICES             :: Int
pattern ERR_MSGSERVICES             = 487

pattern ERR_TSLESSCHAN              :: Int
pattern ERR_TSLESSCHAN              = 488

pattern ERR_VOICENEEDED             :: Int
pattern ERR_VOICENEEDED             = 489

pattern ERR_SECUREONLYCHAN          :: Int
pattern ERR_SECUREONLYCHAN          = 489

pattern ERR_NOOPERHOST              :: Int
pattern ERR_NOOPERHOST              = 491

pattern ERR_NOSERVICEHOST           :: Int
pattern ERR_NOSERVICEHOST           = 492

pattern ERR_NOFEATURE               :: Int
pattern ERR_NOFEATURE               = 493

pattern ERR_BADFEATURE              :: Int
pattern ERR_BADFEATURE              = 494

pattern ERR_BADLOGTYPE              :: Int
pattern ERR_BADLOGTYPE              = 495

pattern ERR_BADLOGSYS               :: Int
pattern ERR_BADLOGSYS               = 496

pattern ERR_BADLOGVALUE             :: Int
pattern ERR_BADLOGVALUE             = 497

pattern ERR_ISOPERLCHAN             :: Int
pattern ERR_ISOPERLCHAN             = 498

pattern ERR_CHANOWNPRIVNEEDED       :: Int
pattern ERR_CHANOWNPRIVNEEDED       = 499

pattern ERR_UMODEUNKNOWNFLAG        :: Int
pattern ERR_UMODEUNKNOWNFLAG        = 501

pattern ERR_USERSDONTMATCH          :: Int
pattern ERR_USERSDONTMATCH          = 502

pattern ERR_GHOSTEDCLIENT           :: Int
pattern ERR_GHOSTEDCLIENT           = 503

pattern ERR_VWORLDWARN              :: Int
pattern ERR_VWORLDWARN              = 503

pattern ERR_USERNOTONSERV           :: Int
pattern ERR_USERNOTONSERV           = 504

pattern ERR_SILELISTFULL            :: Int
pattern ERR_SILELISTFULL            = 511

pattern ERR_TOOMANYWATCH            :: Int
pattern ERR_TOOMANYWATCH            = 512

pattern ERR_BADPING                 :: Int
pattern ERR_BADPING                 = 513

pattern ERR_INVALID_ERROR           :: Int
pattern ERR_INVALID_ERROR           = 514

pattern ERR_TOOMANYDCC              :: Int
pattern ERR_TOOMANYDCC              = 514

pattern ERR_BADEXPIRE               :: Int
pattern ERR_BADEXPIRE               = 515

pattern ERR_DONTCHEAT               :: Int
pattern ERR_DONTCHEAT               = 516

pattern ERR_DISABLED                :: Int
pattern ERR_DISABLED                = 517

pattern ERR_NOINVITE                :: Int
pattern ERR_NOINVITE                = 518

pattern ERR_LONGMASK                :: Int
pattern ERR_LONGMASK                = 518

pattern ERR_ADMONLY                 :: Int
pattern ERR_ADMONLY                 = 519

pattern ERR_TOOMANYUSERS            :: Int
pattern ERR_TOOMANYUSERS            = 519

pattern ERR_OPERONLY                :: Int
pattern ERR_OPERONLY                = 520

pattern ERR_MASKTOOWIDE             :: Int
pattern ERR_MASKTOOWIDE             = 520

pattern ERR_WHOTRUNC                :: Int
pattern ERR_WHOTRUNC                = 520

pattern ERR_LISTSYNTAX              :: Int
pattern ERR_LISTSYNTAX              = 521

pattern ERR_WHOSYNTAX               :: Int
pattern ERR_WHOSYNTAX               = 522

pattern ERR_WHOLIMEXCEED            :: Int
pattern ERR_WHOLIMEXCEED            = 523

pattern ERR_QUARANTINED             :: Int
pattern ERR_QUARANTINED             = 524

pattern ERR_OPERSPVERIFY            :: Int
pattern ERR_OPERSPVERIFY            = 524

pattern ERR_REMOTEPFX               :: Int
pattern ERR_REMOTEPFX               = 525

pattern ERR_PFXUNROUTABLE           :: Int
pattern ERR_PFXUNROUTABLE           = 526

pattern ERR_BADHOSTMASK             :: Int
pattern ERR_BADHOSTMASK             = 550

pattern ERR_HOSTUNAVAIL             :: Int
pattern ERR_HOSTUNAVAIL             = 551

pattern ERR_USINGSLINE              :: Int
pattern ERR_USINGSLINE              = 552

pattern ERR_STATSSLINE              :: Int
pattern ERR_STATSSLINE              = 553

pattern RPL_LOGON                   :: Int
pattern RPL_LOGON                   = 600

pattern RPL_LOGOFF                  :: Int
pattern RPL_LOGOFF                  = 601

pattern RPL_WATCHOFF                :: Int
pattern RPL_WATCHOFF                = 602

pattern RPL_WATCHSTAT               :: Int
pattern RPL_WATCHSTAT               = 603

pattern RPL_NOWON                   :: Int
pattern RPL_NOWON                   = 604

pattern RPL_NOWOFF                  :: Int
pattern RPL_NOWOFF                  = 605

pattern RPL_WATCHLIST               :: Int
pattern RPL_WATCHLIST               = 606

pattern RPL_ENDOFWATCHLIST          :: Int
pattern RPL_ENDOFWATCHLIST          = 607

pattern RPL_WATCHCLEAR              :: Int
pattern RPL_WATCHCLEAR              = 608

pattern RPL_ISOPER                  :: Int
pattern RPL_ISOPER                  = 610

pattern RPL_ISLOCOP                 :: Int
pattern RPL_ISLOCOP                 = 611

pattern RPL_ISNOTOPER               :: Int
pattern RPL_ISNOTOPER               = 612

pattern RPL_ENDOFISOPER             :: Int
pattern RPL_ENDOFISOPER             = 613

pattern RPL_DCCSTATUS               :: Int
pattern RPL_DCCSTATUS               = 617

pattern RPL_DCCLIST                 :: Int
pattern RPL_DCCLIST                 = 618

pattern RPL_ENDOFDCCLIST            :: Int
pattern RPL_ENDOFDCCLIST            = 619

pattern RPL_WHOWASHOST              :: Int
pattern RPL_WHOWASHOST              = 619

pattern RPL_DCCINFO                 :: Int
pattern RPL_DCCINFO                 = 620

pattern RPL_RULES                   :: Int
pattern RPL_RULES                   = 621

pattern RPL_ENDOFO                  :: Int
pattern RPL_ENDOFO                  = 626

pattern RPL_SETTINGS                :: Int
pattern RPL_SETTINGS                = 630

pattern RPL_ENDOFSETTINGS           :: Int
pattern RPL_ENDOFSETTINGS           = 631

pattern RPL_DUMPING                 :: Int
pattern RPL_DUMPING                 = 640

pattern RPL_DUMPRPL                 :: Int
pattern RPL_DUMPRPL                 = 641

pattern RPL_EODUMP                  :: Int
pattern RPL_EODUMP                  = 642

pattern RPL_TRACEROUTE_HOP          :: Int
pattern RPL_TRACEROUTE_HOP          = 660

pattern RPL_TRACEROUTE_START        :: Int
pattern RPL_TRACEROUTE_START        = 661

pattern RPL_MODECHANGEWARN          :: Int
pattern RPL_MODECHANGEWARN          = 662

pattern RPL_CHANREDIR               :: Int
pattern RPL_CHANREDIR               = 663

pattern RPL_SERVMODEIS              :: Int
pattern RPL_SERVMODEIS              = 664

pattern RPL_OTHERUMODEIS            :: Int
pattern RPL_OTHERUMODEIS            = 665

pattern RPL_ENDOF_GENERIC           :: Int
pattern RPL_ENDOF_GENERIC           = 666

pattern RPL_WHOWASDETAILS           :: Int
pattern RPL_WHOWASDETAILS           = 670

pattern RPL_WHOISSECURE             :: Int
pattern RPL_WHOISSECURE             = 671

pattern RPL_UNKNOWNMODES            :: Int
pattern RPL_UNKNOWNMODES            = 672

pattern RPL_CANNOTSETMODES          :: Int
pattern RPL_CANNOTSETMODES          = 673

pattern RPL_LUSERSTAFF              :: Int
pattern RPL_LUSERSTAFF              = 678

pattern RPL_TIMEONSERVERIS          :: Int
pattern RPL_TIMEONSERVERIS          = 679

pattern RPL_NETWORKS                :: Int
pattern RPL_NETWORKS                = 682

pattern RPL_YOURLANGUAGEIS          :: Int
pattern RPL_YOURLANGUAGEIS          = 687

pattern RPL_LANGUAGE                :: Int
pattern RPL_LANGUAGE                = 688

pattern RPL_WHOISSTAFF              :: Int
pattern RPL_WHOISSTAFF              = 689

pattern RPL_WHOISLANGUAGE           :: Int
pattern RPL_WHOISLANGUAGE           = 690

pattern RPL_ENDOFMODLIST            :: Int
pattern RPL_ENDOFMODLIST            = 703

pattern RPL_HELPSTART               :: Int
pattern RPL_HELPSTART               = 704

pattern RPL_HELPTXT                 :: Int
pattern RPL_HELPTXT                 = 705

pattern RPL_ENDOFHELP               :: Int
pattern RPL_ENDOFHELP               = 706

pattern RPL_ETRACEFULL              :: Int
pattern RPL_ETRACEFULL              = 708

pattern RPL_ETRACE                  :: Int
pattern RPL_ETRACE                  = 709

pattern RPL_KNOCK                   :: Int
pattern RPL_KNOCK                   = 710

pattern RPL_KNOCKDLVR               :: Int
pattern RPL_KNOCKDLVR               = 711

pattern ERR_TOOMANYKNOCK            :: Int
pattern ERR_TOOMANYKNOCK            = 712

pattern ERR_CHANOPEN                :: Int
pattern ERR_CHANOPEN                = 713

pattern ERR_KNOCKONCHAN             :: Int
pattern ERR_KNOCKONCHAN             = 714

pattern ERR_KNOCKDISABLED           :: Int
pattern ERR_KNOCKDISABLED           = 715

pattern RPL_TARGUMODEG              :: Int
pattern RPL_TARGUMODEG              = 716

pattern RPL_TARGNOTIFY              :: Int
pattern RPL_TARGNOTIFY              = 717

pattern RPL_UMODEGMSG               :: Int
pattern RPL_UMODEGMSG               = 718

pattern RPL_ENDOFOMOTD              :: Int
pattern RPL_ENDOFOMOTD              = 722

pattern ERR_NOPRIVS                 :: Int
pattern ERR_NOPRIVS                 = 723

pattern RPL_TESTMARK                :: Int
pattern RPL_TESTMARK                = 724

pattern RPL_TESTLINE                :: Int
pattern RPL_TESTLINE                = 725

pattern RPL_NOTESTLINE              :: Int
pattern RPL_NOTESTLINE              = 726

pattern RPL_XINFO                   :: Int
pattern RPL_XINFO                   = 771

pattern RPL_XINFOSTART              :: Int
pattern RPL_XINFOSTART              = 773

pattern RPL_XINFOEND                :: Int
pattern RPL_XINFOEND                = 774

pattern ERR_CANNOTDOCOMMAND         :: Int
pattern ERR_CANNOTDOCOMMAND         = 972

pattern ERR_CANNOTCHANGEUMODE       :: Int
pattern ERR_CANNOTCHANGEUMODE       = 973

pattern ERR_CANNOTCHANGECHANMODE    :: Int
pattern ERR_CANNOTCHANGECHANMODE    = 974

pattern ERR_CANNOTCHANGESERVERMODE  :: Int
pattern ERR_CANNOTCHANGESERVERMODE  = 975

pattern ERR_CANNOTSENDTONICK        :: Int
pattern ERR_CANNOTSENDTONICK        = 976

pattern ERR_UNKNOWNSERVERMODE       :: Int
pattern ERR_UNKNOWNSERVERMODE       = 977

pattern ERR_SERVERMODELOCK          :: Int
pattern ERR_SERVERMODELOCK          = 979

pattern ERR_BADCHARENCODING         :: Int
pattern ERR_BADCHARENCODING         = 980

pattern ERR_TOOMANYLANGUAGES        :: Int
pattern ERR_TOOMANYLANGUAGES        = 981

pattern ERR_NOLANGUAGE              :: Int
pattern ERR_NOLANGUAGE              = 982

pattern ERR_TEXTTOOSHORT            :: Int
pattern ERR_TEXTTOOSHORT            = 983

pattern ERR_NUMERIC_ERR             :: Int
pattern ERR_NUMERIC_ERR             = 999
