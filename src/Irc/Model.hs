{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

-- | This module implements a high-level view of the state of
-- the IRC connection. The library user calls 'advanceModel' to
-- step the 'IrcConnection' as new messages arrive.
module Irc.Model
  ( -- * IRC Connection model
    IrcConnection(..)
  , connNick
  , connChannels
  , connId
  , connChanModeTypes
  , connUserModeTypes
  , connKnock
  , connNickLen
  , connExcepts
  , connInvex
  , connStatusMsg
  , connTopicLen
  , connModes
  , connUsers
  , connMyInfo
  , connSasl
  , connUmode
  , connSnoMask
  , defaultIrcConnection

  -- * IRC Channel model
  , IrcChannel(..)
  , chanTopic
  , chanUsers
  , chanModes
  , chanCreation
  , chanMaskLists
  , chanUrl

  -- * Mode Settings
  , ModeTypes(..)
  , modesLists
  , modesAlwaysArg
  , modesSetArg
  , modesNeverArg
  , modesPrefixModes
  , defaultChanModeTypes
  , defaultUmodeTypes

  -- * Channel Mask Entry
  , IrcMaskEntry(..)
  , maskEntryMask
  , maskEntryWho
  , maskEntryStamp

  -- * User metadata
  , IrcUser(..)
  , usrAway
  , usrAccount
  , usrHost
  , defaultIrcUser

  -- * Model execution
  , runLogic
  , LogicOp(..)
  , Logic

  -- * General functionality
  , advanceModel
  , isChannelName
  , isNickName
  , isMyNick
  , splitStatusMsg
  , splitModes
  , unsplitModes
  ) where

import Control.Applicative
import Control.Monad (guard)
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Free
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Foldable (Foldable)
import Data.List (foldl',find,nub,delete,intersect)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import Data.Time
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import Irc.Format
import Irc.Message
import Irc.Cmd
import Irc.Core
import Irc.Core.Prisms

-- | 'IrcConnection' is the state of an IRC connection. It maintains
-- channel membership, user and channel modes, and other connection
-- state.
data IrcConnection = IrcConnection
  { _connNick     :: Identifier
  , _connChannels :: !(Map Identifier IrcChannel)
  , _connId       :: Maybe ByteString
  , _connChanTypes :: [Char]
  , _connStatusMsg :: [Char]
  , _connKnock    :: Bool
  , _connNickLen  :: Int
  , _connExcepts  :: Maybe Char
  , _connInvex    :: Maybe Char
  , _connUsers    :: !(Map Identifier IrcUser)
  , _connChanModeTypes :: ModeTypes
  , _connUserModeTypes :: ModeTypes
  , _connModes    :: Int
  , _connTopicLen :: Int
  , _connMyInfo   :: Maybe (ByteString,ByteString)
  , _connSasl     :: Maybe (ByteString,ByteString)
  , _connUmode    :: ByteString
  , _connSnoMask  :: ByteString
  , _connPhase    :: Phase
  }
  deriving (Read, Show)

-- | 'IrcConnection' value with everything unspecified
defaultIrcConnection :: IrcConnection
defaultIrcConnection = IrcConnection
  { _connNick      = mkId ""
  , _connChannels  = mempty
  , _connId        = Nothing
  , _connChanTypes = "#&" -- default per RFC
  , _connStatusMsg = ""
  , _connKnock     = False
  , _connNickLen   = 9
  , _connExcepts   = Nothing
  , _connInvex     = Nothing
  , _connUsers     = mempty
  , _connModes     = 3
  , _connTopicLen  = 400 -- default is unbounded but message length is bounded
  , _connChanModeTypes = defaultChanModeTypes
  , _connUserModeTypes = defaultUmodeTypes
  , _connMyInfo    = Nothing
  , _connSasl      = Nothing
  , _connUmode     = ""
  , _connSnoMask   = ""
  , _connPhase     = RegistrationPhase
  }

data Phase
  = RegistrationPhase
  | ActivePhase
  | SaslPhase
  deriving (Read, Show, Eq)

-- | Settings that describe how to interpret channel modes
data ModeTypes = ModeTypes
  { _modesLists :: String
  , _modesAlwaysArg :: String
  , _modesSetArg :: String
  , _modesNeverArg :: String
  , _modesPrefixModes :: [(Char,Char)]
  }
  deriving (Read, Show)

-- | The channel modes used by Freenode
defaultChanModeTypes :: ModeTypes
defaultChanModeTypes = ModeTypes
  { _modesLists     = "eIbq"
  , _modesAlwaysArg = "k"
  , _modesSetArg    = "flj"
  , _modesNeverArg  = "CFLMPQScgimnprstz"
  , _modesPrefixModes = [('o','@'),('v','+')]
  }

-- | The default UMODE as defined by Freenode
defaultUmodeTypes :: ModeTypes
defaultUmodeTypes = ModeTypes
  { _modesLists     = ""
  , _modesAlwaysArg = ""
  , _modesSetArg    = "s"
  , _modesNeverArg  = ""
  , _modesPrefixModes = []
  }

-- | 'IrcChannel' represents the current state of a channel
-- as seen on the connection. It includes all user lists,
-- modes, and other metadata about a channel.
data IrcChannel = IrcChannel
  { _chanTopic :: Maybe (Maybe (Text, ByteString, UTCTime)) -- TODO: use UserInfo
  , _chanUsers :: !(Map Identifier String) -- modes: ov
  , _chanModes :: Maybe (Map Char ByteString)
  , _chanCreation :: Maybe UTCTime
  , _chanMaskLists :: Map Char [IrcMaskEntry]
  , _chanUrl :: Maybe ByteString
  }
  deriving (Read, Show)

-- | Default value for 'IrcChannel' with everything unspecified.
defaultChannel :: IrcChannel
defaultChannel = IrcChannel
  { _chanTopic = Nothing
  , _chanModes = Nothing
  , _chanCreation = Nothing
  , _chanUsers = mempty
  , _chanMaskLists = mempty
  , _chanUrl = Nothing
  }

-- | Mask entries are used to represent an entry in a ban list for
-- a channel.
data IrcMaskEntry = IrcMaskEntry
  { _maskEntryMask  :: ByteString
  , _maskEntryWho   :: ByteString
  , _maskEntryStamp :: UTCTime
  }
  deriving (Read, Show)

-- | 'IrcUser' is the type of user-level metadata tracked for
-- the users visible on the current IRC connection.
data IrcUser = IrcUser
  { _usrAway :: !Bool
  , _usrAccount :: !(Maybe ByteString)
  , _usrHost    :: !(Maybe ByteString)
  }
  deriving (Read,Show)

-- | This represents the metadata of an unknown user.
defaultIrcUser :: IrcUser
defaultIrcUser = IrcUser
  { _usrAway    = False
  , _usrAccount = Nothing
  , _usrHost    = Nothing
  }

data Fuzzy a = Known !a | Unknown | None
  deriving (Read,Show,Functor,Foldable,Traversable)

makeLenses ''IrcConnection
makeLenses ''IrcChannel
makeLenses ''IrcUser
makeLenses ''IrcMaskEntry
makeLenses ''ModeTypes


-- | Primary state machine step function. Call this function with a timestamp
-- and a server message to update the 'IrcConnection' state. If additional
-- messages are required they will be requested via the 'Logic' type.
advanceModel :: MsgFromServer -> IrcConnection -> Logic IrcConnection
advanceModel msg0 conn =
  case msg0 of
       Ping x -> sendMessage (pongCmd x) >> return conn

       Pong server mbMsg ->
         doServerMessage "PONG" (server <> maybe "" (" "<>) mbMsg) conn

       RplWelcome  txt -> doServerMessage "Welcome" txt
                        $ set connPhase ActivePhase conn
       RplYourHost txt -> doServerMessage "YourHost" txt conn
       RplCreated  txt -> doServerMessage "Created" txt conn
       RplMyInfo host version _ _ _ ->
         return (set connMyInfo (Just (host,version)) conn)

       -- Random uninteresting statistics
       RplLuserOp _         -> return conn
       RplLuserChannels _   -> return conn
       RplLuserMe _         -> return conn
       RplLuserClient _     -> return conn
       RplLocalUsers _      -> return conn
       RplGlobalUsers _     -> return conn
       RplStatsConn _       -> return conn
       RplLuserUnknown _    -> return conn

       RplLuserAdminMe    txt ->
         doServerMessage "ADMIN" txt conn
       RplLuserAdminLoc1  txt ->
         doServerMessage "ADMIN" txt conn
       RplLuserAdminLoc2  txt ->
         doServerMessage "ADMIN" txt conn
       RplLuserAdminEmail txt ->
         doServerMessage "ADMIN" txt conn

       -- Channel list not implemented
       RplListStart     -> return conn
       RplList chan count topic -> doList chan count topic conn
       RplListEnd       -> return conn

       RplUserHost host ->
         doServerMessage "USERHOST" (B8.unwords host) conn

       RplTime server time -> doServerMessage "TIME" (B8.unwords [server,time]) conn

       RplInfo _ -> return conn
       RplEndOfInfo -> return conn

       Join who chan -> doJoinChannel who Unknown chan conn
       ExtJoin who chan account _realname -> doJoinChannel who (maybe None Known account) chan conn
       Part who chan reason -> doPart who chan reason conn
       Kick who chan tgt reason -> doKick who chan tgt reason conn
       Quit who reason -> doQuit who reason conn
       Nick who newnick -> doNick who newnick conn

       RplChannelUrl chan url ->
            return (set (connChannels . ix chan . chanUrl)
                        (Just url)
                        conn)

       RplNoTopicSet chan ->
          return (set (connChannels . ix chan . chanTopic)
                      (Just Nothing)
                      conn)

       RplTopic chan topic ->
         do RplTopicWhoTime _ who time <- getMessage
            return (set (connChannels . ix chan . chanTopic)
                        (Just (Just (asUtf8 topic,who,time)))
                        conn)
       RplTopicWhoTime _ _ _ ->
          fail "Unexpected RPL_TOPICWHOTIME"

       Topic who chan topic -> doTopic who chan topic conn

       PrivMsg who chan msg -> doPrivMsg who chan msg conn

       Notice who chan msg -> doNotifyChannel who chan msg conn

       Account who acct ->
         return (set (connUsers . ix (userNick who) . usrAccount) acct conn)

       Away who _msg ->
         return (updateUserRecord (userNick who) (set usrAway True) conn)

       RplYourId yourId -> return (set connId (Just yourId) conn)

       RplMotdStart -> return conn
       RplEndOfMotd -> return conn
       RplMotd x    -> doServerMessage "MOTD" x conn

       RplNameReply _ chan xs -> doNameReply chan xs conn
       RplEndOfNames _ -> return conn

       RplChannelModeIs chan modes params -> doChannelModeIs chan modes params conn

       RplCreationTime chan creation ->
         return (set (connChannels . ix chan . chanCreation) (Just creation) conn)

       RplWhoReply _chan _username hostname _servername nickname flags _realname ->
         doWhoReply nickname hostname flags conn

       RplEndOfWho _chan -> return conn

       RplIsOn nicks -> return (doIsOn nicks conn)

       RplBanList chan mask who when ->
         doMaskList (preview _RplBanList)
                    (has _RplEndOfBanList)
                    'b' chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfBanList chan ->
         return (set (connChannels . ix chan . chanMaskLists . at 'b') (Just []) conn)

       RplInviteList chan mask who when ->
         doMaskList (preview _RplInviteList)
                    (has _RplEndOfInviteList)
                    'I' chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfInviteList chan ->
         return (set (connChannels . ix chan . chanMaskLists . at 'I') (Just []) conn)

       RplExceptionList chan mask who when ->
         doMaskList (preview _RplExceptionList)
                    (has _RplEndOfExceptionList)
                    'e' chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfExceptionList chan ->
         return (set (connChannels . ix chan . chanMaskLists . at 'e') (Just []) conn)

       RplQuietList chan mode mask who when ->
         let fixup (a,_,c,d,e) = (a,c,d,e) in -- drop the matched mode field
         doMaskList (previews _RplQuietList fixup)
                    (has _RplEndOfQuietList)
                    mode chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfQuietList chan mode ->
         return (set (connChannels . ix chan . chanMaskLists . at mode) (Just []) conn)

       Mode _ _ [] -> fail "Unexpected MODE"
       Mode who target (modes:args) ->
         doModeChange who target modes args conn

       RplSnoMask snomask ->
         return (set connSnoMask snomask conn)

       RplUmodeIs mode _params -> -- TODO: params?
         return (set connUmode (B.tail mode) conn)

       Err target err ->
         do now <- getStamp
            let mesg = defaultIrcMessage
                  { _mesgType    = ErrMsgType err
                  , _mesgSender  = UserInfo "" Nothing Nothing
                  , _mesgStamp   = now
                  }
            recordMessage mesg target conn

       RplKnockDelivered chan ->
         doChannelError chan "Knock delivered" conn
       RplKnock chan who ->
         do now <- getStamp
            let mesg = defaultIrcMessage
                  { _mesgType    = KnockMsgType
                  , _mesgSender  = who
                  , _mesgStamp   = now
                  }
            recordMessage mesg chan conn

       RplInviting nick chan ->
         doChannelError chan ("Inviting " <> asUtf8 (idBytes nick)) conn
       Invite who chan ->
         do now <- getStamp
            let mesg = defaultIrcMessage
                  { _mesgType    = InviteMsgType
                  , _mesgSender  = who
                  , _mesgStamp   = now
                  }
            recordMessage mesg chan conn

       -- TODO: Structure this more nicely than as simple message,
       -- perhaps store it in the user map
       RplWhoisUser nick user host real ->
         doServerMessage "WHOIS" (B8.unwords [idBytes nick, user, host, real])
            (updateUserRecord nick (set usrHost (Just host)) conn)
       RplWhoisChannels _nick channels ->
         doServerMessage "WHOIS" channels conn
       RplWhoisServer _nick host txt ->
         doServerMessage "WHOIS" (B8.unwords [host,txt]) conn
       RplWhoisSecure _nick ->
         doServerMessage "WHOIS" "secure connection" conn
       RplWhoisHost _nick txt ->
         doServerMessage "WHOIS" txt conn
       RplWhoisIdle _nick idle signon ->
         doServerMessage "WHOIS" ("Idle seconds: " <> B8.pack (show idle) <>
                                  ", Sign-on: " <> maybe "unknown" (B8.pack . show)
                                                        signon
                                 ) conn
       RplWhoisAccount nick account ->
         doServerMessage "WHOIS" ("Logged in as: " <> account)
            (set (connUsers . ix nick . usrAccount) (Just account) conn)
       RplWhoisModes _nick modes args ->
         doServerMessage "WHOIS" ("Modes: " <> B8.unwords (modes:args)) conn
       RplWhoisOperator _nick txt  ->
         doServerMessage "WHOIS" ("Operator: " <> txt) conn
       RplWhoisCertFp _nick txt  ->
         doServerMessage "WHOIS" ("CertFP: " <> txt) conn
       RplEndOfWhois _nick ->
         doServerMessage "WHOIS" "--END--" conn

       RplAway nick message ->
         doAwayReply nick (asUtf8 message) conn
       RplUnAway ->
         doServerMessage "AWAY" "You are no longer marked away" conn
       RplNowAway ->
         doServerMessage "AWAY" "You are marked away" conn

       RplWhoWasUser nick user host real ->
         doServerMessage "WHOWAS" (B8.unwords [idBytes nick, user, host, real]) conn
       RplEndOfWhoWas _nick ->
         doServerMessage "WHOWAS" "--END--" conn

       RplHostHidden host ->
         doServerMessage "HOST" ("Host hidden: " <> host) conn
       RplYoureOper txt ->
         doServerMessage "OPER" txt conn

       RplHelpStart topic txt -> doServerMessage topic txt conn
       RplHelp      topic txt -> doServerMessage topic txt conn
       RplEndOfHelp topic     -> doServerMessage topic "--END--" conn

       Cap "LS" caps -> doCapLs caps conn
       Cap "ACK" caps -> doCapAck caps conn
       Cap "NACK" _caps -> sendMessage capEndCmd >> return conn
       Cap _ _ -> fail "Unexpected CAP"
       RplSaslAborted -> return conn

       RplLoadTooHigh cmd ->
         doServerError ("Command rate limited: " <> asUtf8 cmd) conn
       RplNickLocked ->
         doServerError "Nickname locked" conn
       RplLoggedIn account ->
         doServerMessage "LOGIN" account conn
       RplLoggedOut ->
         doServerMessage "LOGOUT" "" conn
       RplSaslTooLong ->
         doServerError "Unexpected SASL Too Long" conn
       RplSaslAlready ->
         doServerError "Unexpected SASL Already" conn
       RplSaslMechs _ ->
         doServerError "Unexpected SASL Mechanism List" conn

       Error e -> doServerError (asUtf8 e) conn

       RplISupport isupport -> doISupport isupport conn
       RplVersion version ->
         doServerMessage "VERSION" (B8.unwords version) conn

       RplUmodeGMsg nick mask -> doCallerId nick mask conn
       RplTargNotify nick -> doCallerIdDeliver nick conn

       RplAcceptList nick -> doAcceptList [nick] conn
       RplEndOfAccept -> doServerMessage "ACCEPTLIST" "Accept list empty" conn

       RplLinks mask server info -> doServerMessage "LINKS" (B8.unwords [mask,server,info]) conn
       RplEndOfLinks mask -> doServerMessage "LINKS" mask conn

       RplStatsLinkInfo linkinfo -> doServerMessage "LINKINFO" (B8.unwords linkinfo) conn
       RplStatsCommands commands -> doServerMessage "COMMANDS" (B8.unwords commands) conn
       RplStatsCLine cline -> doServerMessage "CLINE" (B8.unwords cline) conn
       RplStatsNLine nline -> doServerMessage "NLINE" (B8.unwords nline) conn
       RplStatsILine iline -> doServerMessage "ILINE" (B8.unwords iline) conn
       RplStatsKLine kline -> doServerMessage "KLINE" (B8.unwords kline) conn
       RplStatsQLine qline -> doServerMessage "QLINE" (B8.unwords qline) conn
       RplStatsYLine yline -> doServerMessage "YLINE" (B8.unwords yline) conn
       RplEndOfStats mode -> doServerMessage "ENDSTATS" (B8.pack [mode]) conn
       RplStatsPLine pline -> doServerMessage "PLINE" (B8.unwords pline) conn
       RplStatsDLine dline -> doServerMessage "DLINE" (B8.unwords dline) conn
       RplStatsVLine vline -> doServerMessage "VLINE" (B8.unwords vline) conn
       RplStatsLLine lline -> doServerMessage "LLINE" (B8.unwords lline) conn
       RplStatsUptime uptime -> doServerMessage "UPTIME" uptime conn
       RplStatsOLine oline -> doServerMessage "OLINE" (B8.unwords oline) conn
       RplStatsHLine hline -> doServerMessage "HLINE" (B8.unwords hline) conn
       RplStatsSLine sline -> doServerMessage "SLINE" (B8.unwords sline) conn
       RplStatsPing  ping  -> doServerMessage "STATSPING" (B8.unwords ping) conn
       RplStatsXLine xline -> doServerMessage "XLINE" (B8.unwords xline) conn
       RplStatsULine uline -> doServerMessage "ULINE" (B8.unwords uline) conn
       RplStatsDebug debug -> doServerMessage "STATSDEBUG" (B8.unwords debug) conn

       RplPrivs txt -> doServerMessage "PRIVS" txt conn

       Authenticate msg
         | view connPhase conn == SaslPhase
         , msg == "+"
         , Just (user,pass) <- view connSasl conn ->
             do sendMessage (authenticateCmd (encodePlainAuthentication user pass))
                return conn
         | otherwise ->
             doServerError "Unexpected Authenticate" conn

       RplSaslSuccess
         | view connPhase conn == SaslPhase ->
            do sendMessage capEndCmd
               doServerMessage "SASL" "Authentication successful"
                  $ set connPhase RegistrationPhase conn
         | otherwise ->
             doServerError "Unexpected SASL Success" conn

       RplSaslFail
         | view connPhase conn == SaslPhase ->
            do sendMessage capEndCmd
               doServerMessage "SASL" "Authentication failed"
                   $ set connPhase RegistrationPhase conn
         | otherwise ->
            doServerError "Unexpected SASL Fail" conn




-- ISUPPORT is defined by
-- https://tools.ietf.org/html/draft-brocklesby-irc-isupport-03#section-3.14
doISupport ::
  [(ByteString,ByteString)] {- ^ [(key,value)] -} ->
  IrcConnection -> Logic IrcConnection
doISupport params conn = return (foldl' (flip support) conn params)

support :: (ByteString,ByteString) -> IrcConnection -> IrcConnection
support ("CHANTYPES",types) = set connChanTypes (B8.unpack types)
support ("CHANMODES",modes) = updateChanModes (B8.unpack modes)
support ("STATUSMSG",modes) = set connStatusMsg (B8.unpack modes)
support ("PREFIX",modes) = updateChanPrefix (B8.unpack modes)
support ("KNOCK",_) = set connKnock True
support ("NICKLEN",len) =
  case B8.readInt len of
    Just (n,rest) | B.null rest -> set connNickLen n
    _                           -> id

support ("TOPICLEN",len) =
  case B8.readInt len of
    Just (n,rest) | B.null rest -> set connTopicLen n
    _                           -> id

support ("MODES",str) =
  case B8.readInt str of
    Just (n,rest) | B.null rest -> set connModes (max 1 n)
    _                           -> id

support ("INVEX",mode) =
  case B8.uncons mode of
    Nothing    -> set connInvex (Just 'I')
    Just (m,_) -> set connInvex (Just $! m)

support ("EXCEPTS",mode) =
  case B8.uncons mode of
    Nothing    -> set connExcepts (Just 'e')
    Just (m,_) -> set connExcepts (Just $! m)

support _ = id

updateChanModes ::
  String {- lists,always,set,never -} ->
  IrcConnection -> IrcConnection
updateChanModes modes
  = over connChanModeTypes
  $ set modesLists listModes
  . set modesAlwaysArg alwaysModes
  . set modesSetArg setModes
  . set modesNeverArg neverModes
  where
  next = over _2 (drop 1) . break (==',')
  (listModes  ,modes1) = next modes
  (alwaysModes,modes2) = next modes1
  (setModes   ,modes3) = next modes2
  (neverModes ,_)      = next modes3

updateChanPrefix ::
  String {- e.g. (ov)@+ -} ->
  IrcConnection -> IrcConnection
updateChanPrefix [] = id
updateChanPrefix (_:modes) =
  set (connChanModeTypes . modesPrefixModes) (zip a b)
  where
  (a,b) = over _2 (drop 1) (break (==')') modes)

doAcceptList ::
  [Identifier] {- ^ nicks -} ->
  IrcConnection -> Logic IrcConnection
doAcceptList acc conn =
  do msg <- getMessage
     case msg of
       RplAcceptList nick -> doAcceptList (nick:acc) conn
       RplEndOfAccept     -> doServerMessage "ACCEPTLIST"
                                (B8.unwords (map idBytes (reverse acc))) conn
       _ -> fail "doAcceptList: Unexpected message!"

doCallerIdDeliver ::
  Identifier {- ^ nick -} ->
  IrcConnection -> Logic IrcConnection
doCallerIdDeliver nick conn =
  do stamp <- getStamp
     let mesg = defaultIrcMessage
           { _mesgType    = CallerIdDeliveredMsgType
           , _mesgSender  = UserInfo nick Nothing Nothing
           , _mesgStamp   = stamp
           }
     recordMessage mesg nick conn

doCallerId ::
  Identifier {- ^ nick -} ->
  ByteString {- ^ user\@host -} ->
  IrcConnection -> Logic IrcConnection
doCallerId nick mask conn =
  do stamp <- getStamp
     let (user,host) = B8.break (=='@') mask
     let mesg = defaultIrcMessage
           { _mesgType    = CallerIdMsgType
           , _mesgSender  = UserInfo nick (Just user) (Just (B8.drop 1 host))
           , _mesgStamp   = stamp
           }
     recordMessage mesg nick conn

doList ::
  Identifier {- ^ channel -} ->
  Integer    {- ^ members -} ->
  ByteString {- ^ topic   -} ->
  IrcConnection -> Logic IrcConnection
doList chan num topic =
  doServerMessage "LIST"
    (B8.unwords [idBytes chan, " - ",
                 B8.pack (show num), " - ",
                 topic])

doAwayReply ::
  Identifier {- ^ nickname -} ->
  Text       {- ^ away message -} ->
  IrcConnection -> Logic IrcConnection
doAwayReply nick message conn =
  do stamp <- getStamp
     let mesg = defaultIrcMessage
           { _mesgType    = AwayMsgType message
           , _mesgSender  = UserInfo nick Nothing Nothing
           , _mesgStamp   = stamp
           }
     recordMessage mesg nick conn

doChannelError ::
  Identifier {- ^ channel -} ->
  Text       {- ^ error   -} ->
  IrcConnection -> Logic IrcConnection
doChannelError chan reason conn =
  do stamp <- getStamp
     let mesg = defaultIrcMessage
           { _mesgType    = ErrorMsgType reason
           , _mesgSender  = UserInfo (mkId "server") Nothing Nothing
           , _mesgStamp   = stamp
           }
     recordMessage mesg chan conn

-- | Event handler when receiving a new privmsg.
-- The message will be passed along as an event.
doPrivMsg ::
  UserInfo   {- ^ sender         -} ->
  Identifier {- ^ message target -} ->
  ByteString {- ^ message        -} ->
  IrcConnection -> Logic IrcConnection
doPrivMsg who chan msg conn =
  do stamp <- getStamp
     let (statusmsg, chan') = splitStatusMsg chan conn
         mesg = defaultIrcMessage
                { _mesgType    = ty
                , _mesgSender  = who
                , _mesgStamp   = stamp
                , _mesgStatus  = statusmsg
                }
         ty = case parseCtcpCommand msg of
                Nothing                 -> PrivMsgType (asUtf8 msg)
                Just ("ACTION", action) -> ActionMsgType (asUtf8 action)
                Just (command , args  ) -> CtcpReqMsgType command args

     recordMessage mesg chan' conn

parseCtcpCommand :: ByteString -> Maybe (ByteString, ByteString)
parseCtcpCommand msg
  | B8.length msg >= 3
  , B8.head   msg == '\^A'
  , B8.last   msg == '\^A' = Just (B8.map toUpper command, B8.drop 1 rest)
  | otherwise              = Nothing
  where
  sansControls = B8.tail (B8.init msg)
  (command,rest) = B8.break (==' ') sansControls

-- | Record the new topic as set by the given user and
-- emit a change event.
doTopic ::
  UserInfo  {- ^ changed by -} ->
  Identifier {- ^ channel -} ->
  ByteString {- ^ topic text -} ->
  IrcConnection -> Logic IrcConnection
doTopic who chan topic conn =
  do stamp <- getStamp
     let topicText = asUtf8 topic
         m = defaultIrcMessage
                { _mesgType = TopicMsgType topicText
                , _mesgSender = who
                , _mesgStamp = stamp
                }
         topicEntry = Just (topicText,renderUserInfo who,stamp)
         conn1 = set (connChannels . ix chan . chanTopic) (Just topicEntry) conn
     recordMessage m chan conn1

doCapLs :: ByteString -> IrcConnection -> Logic IrcConnection
doCapLs rawCaps conn =
  do sendMessage (capReqCmd activeCaps)
     return conn
  where
  activeCaps = intersect supportedCaps offeredCaps
  offeredCaps = B8.words rawCaps
  supportedCaps = saslSupport
               ++ ["away-notify","account-notify","userhost-in-names",
                   "extended-join","multi-prefix",
                   "znc.in/server-time-iso","server-time"]
  saslSupport =
    case view connSasl conn of
      Nothing -> []
      Just{}  -> ["sasl"]

doCapAck ::
  ByteString {- ^ raw, spaces delimited caps list -} ->
  IrcConnection -> Logic IrcConnection
doCapAck rawCaps conn =
  let ackCaps = B8.words rawCaps in
  case view connSasl conn of
    Just{} | "sasl" `elem` ackCaps ->
         do sendMessage (authenticateCmd "PLAIN")
            return (set connPhase SaslPhase conn)
    _ -> do sendMessage capEndCmd
            return conn

encodePlainAuthentication ::
  ByteString {- ^ username -} ->
  ByteString {- ^ password -} ->
  ByteString
encodePlainAuthentication user pass
  = Base64.encode
  $ B8.intercalate "\0" [user,user,pass]


doChannelModeIs ::
  Identifier {- ^ Channel -} ->
  ByteString {- ^ modes -} ->
  [ByteString] {- ^ mode parameters -} ->
  IrcConnection -> Logic IrcConnection
doChannelModeIs chan modes args conn =
  case splitModes (view connChanModeTypes conn) modes args of
    Nothing -> fail "Bad mode string"
    Just xs -> return (set (connChannels . ix chan . chanModes) (Just modeMap) conn)
      where
      modeMap = Map.fromList [ (mode,arg) | (True,mode,arg) <- xs ]

doServerError :: Text -> IrcConnection -> Logic IrcConnection
doServerError err conn =
  do stamp <- getStamp
     let mesg = defaultIrcMessage
           { _mesgType    = ErrorMsgType err
           , _mesgSender  = UserInfo (mkId "server") Nothing Nothing
           , _mesgStamp   = stamp
           }
     recordFor "" mesg
     return conn

-- | Mark all the given nicks as active (not-away).
doIsOn ::
  [Identifier] {- ^ active nicks -} ->
  IrcConnection -> IrcConnection
doIsOn nicks conn = foldl' setIsOn conn nicks
  where
  setIsOn connAcc nick = updateUserRecord nick (set usrAway False) connAcc

doModeChange ::
  UserInfo     {- ^ who           -} ->
  Identifier   {- ^ target        -} ->
  ByteString   {- ^ modes changed -} ->
  [ByteString] {- ^ arguments     -} ->
  IrcConnection -> Logic IrcConnection
doModeChange who target modes0 args0 conn
  | isChannelName target conn =
      case splitModes modeSettings modes0 args0 of
        Nothing -> return conn
        Just ms -> doChannelModeChanges ms who target conn

  -- TODO: Implement user modes
  | otherwise =
      case splitModes (view connUserModeTypes conn) modes0 args0 of
        Nothing -> return conn
        Just ms -> return (doUserModeChanges ms conn)

  where
  modeSettings = view connChanModeTypes conn

doUserModeChanges ::
  [(Bool, Char, ByteString)] {- ^ [(+/-,mode,argument)] -} ->
  IrcConnection -> IrcConnection
doUserModeChanges ms =
  over connUmode addModes
  where
  addModes bs = B.sort (foldl' aux bs ms)
  aux bs (polarity,m,_)
    | polarity && B8.elem m bs = bs
    | polarity                 = B8.cons m bs
    | otherwise                = B8.filter (/= m) bs

doChannelModeChanges ::
  [(Bool, Char, ByteString)] {- ^ [(+/-,mode,argument)] -} ->
  UserInfo                   {- ^ changer               -} ->
  Identifier                 {- ^ channel               -} ->
  IrcConnection -> Logic IrcConnection
doChannelModeChanges ms who chan conn0 =
  do now <- getStamp
     foldM (aux now) conn0 ms
  where
  settings = view connChanModeTypes conn0

  aux now conn (polarity,m,a)
    = fmap (over (connChannels . ix chan)
                 (installModeChange settings now who polarity m a))
           (recordMessage modeMsg chan conn)
    where
    modeMsg = defaultIrcMessage
           { _mesgType   = ModeMsgType polarity m a
           , _mesgSender = who
           , _mesgStamp  = now
           , _mesgModes  = view ( connChannels . ix chan
                                . chanUsers . ix (userNick who)
                                ) conn
           }

installModeChange ::
  ModeTypes  {- ^ settings -} ->
  UTCTime    {- ^ timestamp -} ->
  UserInfo   {- ^ changer -} ->
  Bool       {- ^ +/-     -} ->
  Char       {- ^ mode    -} ->
  ByteString {- ^ argument -} ->
  IrcChannel -> IrcChannel
installModeChange settings now who polarity mode arg


  -- Handle bans, exceptions, invex, quiets
  | mode `elem` view modesLists settings =
      if polarity
         then over (chanMaskLists . ix mode)
                   (cons (IrcMaskEntry arg (renderUserInfo who) now))

         else over (chanMaskLists . ix mode)
                   (filter (\x -> ircFoldCase (view maskEntryMask x)
                               /= ircFoldCase arg))

  -- Handle ops and voices
  | mode `elem` views modesPrefixModes (map fst) settings =
      if polarity
         then over (chanUsers . ix (mkId arg))
                   (nub . cons mode)

         else over (chanUsers . ix (mkId arg))
                   (delete mode)

  | otherwise =
      if polarity
         then set (chanModes . mapped . at mode)
                  (Just arg)

         else set (chanModes . mapped . at mode)
                  Nothing

unsplitModes ::
  [(Bool,Char,ByteString)] ->
  [ByteString]
unsplitModes modes
  = B8.pack (foldr combineModeChars (const "") modes True)
  : [arg | (_,_,arg) <- modes, not (B.null arg)]
  where
  combineModeChars (q,m,_) rest p
    | p == q = m : rest p
    | q = '+' : m : rest True
    | otherwise = '-' : m : rest False


-- | Split up a mode change command and arguments into individual changes
-- given a configuration.
splitModes ::
  ModeTypes {- ^ mode interpretation -} ->
  ByteString   {- ^ modes               -} ->
  [ByteString] {- ^ arguments           -} ->
  Maybe [(Bool,Char,ByteString)]
splitModes icm modes0 =
  foldr aux (\_ args -> [] <$ guard (null args)) (B8.unpack modes0) True
  where
  aux m rec polarity args =
    case m of
      '+' -> rec True  args
      '-' -> rec False args

      _ |             m `elem` view modesAlwaysArg icm
       || polarity && m `elem` view modesSetArg icm
       ||             m `elem` views modesPrefixModes (map fst) icm
       ||             m `elem` view modesLists icm ->
           do x:xs <- Just args
              fmap (cons (polarity,m,x)) (rec polarity xs)

        | otherwise -> -- default to no arg
              fmap (cons (polarity,m,"")) (rec polarity args)

doMaskList ::
  (MsgFromServer -> Maybe (Identifier,ByteString,ByteString,UTCTime)) ->
  (MsgFromServer -> Bool) ->
  Char ->
  Identifier ->
  [IrcMaskEntry] ->
  IrcConnection -> Logic IrcConnection
doMaskList matchEntry matchEnd mode chan acc conn =
  do msg <- getMessage
     case matchEntry msg of
       Just (_,mask,who,stamp) ->
         doMaskList
           matchEntry matchEnd
           mode chan
           (IrcMaskEntry
               { _maskEntryMask  = mask
               , _maskEntryWho   = who
               , _maskEntryStamp = stamp
               } : acc)
           conn

       _ | matchEnd msg ->
           return (set (connChannels . ix chan . chanMaskLists . at mode)
                       (Just (reverse acc))
                       conn)

       _ -> fail "Expected mode list end"


-- | Update an 'IrcConnection' when a user changes nicknames.
doNick ::
  UserInfo   {- ^ old user infomation -} ->
  Identifier {- ^ new nickname        -} ->
  IrcConnection -> Logic IrcConnection
doNick who newnick conn =
  do stamp <- getStamp
     let m = defaultIrcMessage
                { _mesgType = NickMsgType newnick
                , _mesgSender = who
                , _mesgStamp = stamp
                }

     let conn1 | isMyNick (userNick who) conn =
                        set connNick newnick conn
               | otherwise = conn

         conn2 = set (connUsers . at newnick)
                     (view (connUsers . at (userNick who)) conn1)
               $ set (connUsers . at (userNick who))
                     Nothing
               $ conn1

     iforOf (connChannels . itraversed) conn2 (updateChannel m)

  where
  oldnick = userNick who

  updateChannel :: IrcMessage -> Identifier -> IrcChannel -> Logic IrcChannel
  updateChannel m tgt chan
    | has (chanUsers . ix oldnick) chan
     = do recordFor tgt m
          pure $ set (chanUsers . at oldnick) Nothing
               $ set (chanUsers . at newnick) (view (chanUsers . at oldnick) chan)
               $ chan
    | otherwise = pure chan


-- | Update the 'IrcConnection' when a user parts from a channel.
doPart ::
  UserInfo   {- ^ user information -} ->
  Identifier {- ^ channel          -} ->
  ByteString {- ^ part reason      -} ->
  IrcConnection -> Logic IrcConnection
doPart who chan reason conn =
  do stamp <- getStamp
     let mesg = defaultIrcMessage
                { _mesgType = PartMsgType (asUtf8 reason)
                , _mesgSender = who
                , _mesgStamp = stamp
                }
         removeUser = set (chanUsers . at (userNick who)) Nothing

     conn1 <- fmap (over (connChannels . ix chan) removeUser)
                   (recordMessage mesg chan conn)

     let stillKnown = has (connChannels . folded . chanUsers . ix (userNick who))
                          conn1

         conn2 | stillKnown = conn1
               | otherwise  = set (connUsers . at (userNick who)) Nothing conn1

         conn3 | isMyNick (userNick who) conn =
                    set (connChannels . at chan) Nothing conn2
               | otherwise = conn2

     return conn3

-- | Update an 'IrcConnection' when a user is kicked from a channel.
doKick ::
  UserInfo   {- ^ kicker      -} ->
  Identifier {- ^ channel     -} ->
  Identifier {- ^ kicked      -} ->
  ByteString {- ^ kick reason -} ->
  IrcConnection -> Logic IrcConnection
doKick who chan tgt reason conn =
  do stamp <- getStamp
     let mesg = defaultIrcMessage
                { _mesgType = KickMsgType tgt (asUtf8 reason)
                , _mesgSender = who
                , _mesgStamp = stamp
                }

     let conn1 = set (connChannels . ix chan . chanUsers . at tgt)
                     Nothing
                     conn

         stillKnown = has (connChannels . folded . chanUsers . ix (userNick who))
                          conn1

         conn2 | stillKnown = conn1
               | otherwise  = set (connUsers . at (userNick who)) Nothing conn1

         conn3
           | isMyNick tgt conn2 =
                set (connChannels . at chan) Nothing conn2
           | otherwise = conn2

     recordMessage mesg chan conn3

updateUserRecord :: Identifier -> (IrcUser -> IrcUser) -> IrcConnection -> IrcConnection
updateUserRecord nick f conn =
  case view (connUsers . at nick) conn of
    Nothing -> conn
    Just old -> (set (connUsers . ix nick) $! f old) conn

doWhoReply :: Identifier -> ByteString -> ByteString -> IrcConnection -> Logic IrcConnection
doWhoReply nickname hostname flags conn =
  return $! updateUserRecord
              nickname
              (set usrAway away . updateHost)
              conn
  where
  away = not (B.null flags) && B.take 1 flags == "G"

  updateHost = set usrHost (Just hostname)

-- | Update an 'IrcConnection' with the quitting of a user.
doQuit ::
  UserInfo   {- ^ user info   -} ->
  ByteString {- ^ quit reason -} ->
  IrcConnection -> Logic IrcConnection
doQuit who reason conn =
  do stamp <- getStamp
     let mesg = defaultIrcMessage
                { _mesgType = QuitMsgType (asUtf8 reason)
                , _mesgSender = who
                , _mesgStamp = stamp
                }

     iforOf (connChannels . itraversed)
           (set (connUsers . at (userNick who)) Nothing conn)
           $ \tgt chan ->
       if has (chanUsers . ix (userNick who)) chan
         then do recordFor tgt mesg
                 pure (set (chanUsers . at (userNick who)) Nothing chan)
         else pure chan


doJoinChannel ::
  UserInfo         {- ^ who joined   -} ->
  Fuzzy ByteString {- ^ account name -} ->
  Identifier       {- ^ channel      -} ->
  IrcConnection -> Logic IrcConnection
doJoinChannel who acct chan conn =
  do stamp <- getStamp

     -- add channel if necessary
     let conn1
           | isMyNick (userNick who) conn =
               set (connChannels . at chan) (Just defaultChannel) conn
           | otherwise = conn

     -- add user to channel
         conn2 = set (connChannels . ix chan . chanUsers . at (userNick who))
                     (Just "") -- empty modes
                     conn1

         conn3 = recordAccount (learnUserInfo who (ensureRecord conn2))

     -- update user record
         ensureRecord = over (connUsers . at (userNick who)) (Just . fromMaybe defaultIrcUser)

         recordAccount = case acct of
                   None -> over (connUsers . ix (userNick who))
                                (set usrAccount Nothing)
                   Known a -> over (connUsers . ix (userNick who))
                                (set usrAccount (Just a))
                   Unknown -> id

     -- record join event
         m = defaultIrcMessage
               { _mesgType = JoinMsgType
               , _mesgSender = who
               , _mesgStamp = stamp
               }
     recordMessage m chan conn3

learnUserInfo :: UserInfo -> IrcConnection -> IrcConnection
learnUserInfo ui conn =
  case userHost ui of
    Nothing -> conn
    Just host ->
      let update Nothing = Just defaultIrcUser { _usrHost = Just host }
          update (Just u) = Just $! set usrHost (Just host) u
      in over (connUsers . at (userNick ui)) update conn


doNotifyChannel ::
  UserInfo ->
  Identifier ->
  ByteString ->
  IrcConnection ->
  Logic IrcConnection

doNotifyChannel who chan msg conn =
  do stamp <- getStamp
     let (statusmsg, chan') = splitStatusMsg chan conn
     let ty = case parseCtcpCommand msg of
                Nothing                 -> NoticeMsgType (asUtf8 msg)
                Just (command , args  ) -> CtcpRspMsgType command args
     let mesg = defaultIrcMessage
               { _mesgType = ty
               , _mesgSender = who
               , _mesgStamp = stamp
               , _mesgStatus = statusmsg
               }
     recordMessage mesg chan' conn

doServerMessage ::
  ByteString {- ^ who -} ->
  ByteString {- ^ message -} ->
  IrcConnection -> Logic IrcConnection
doServerMessage who txt conn =
  do stamp <- getStamp
     let m = defaultIrcMessage
               { _mesgType    = PrivMsgType (asUtf8 txt)
               , _mesgSender  = UserInfo (mkId who) Nothing Nothing
               , _mesgStamp   = stamp
               }
     recordFor "" m
     return conn

doNameReply :: Identifier -> [ByteString] -> IrcConnection -> Logic IrcConnection
doNameReply chan xs conn =
  do msg <- getMessage
     case msg of
       RplNameReply _ _ x -> doNameReply chan (x++xs) conn
       RplEndOfNames _ -> return
                     $ learnAllHosts
                     $ set (connChannels . ix chan . chanUsers)
                           users
                           conn
       _ -> fail "Expected end of names"
       where
       modeMap = view (connChanModeTypes . modesPrefixModes) conn

       splitNames :: [(UserInfo, String)]
       splitNames = map (splitNamesReplyName modeMap) xs

       users :: Map Identifier String
       users = Map.fromList (map (over _1 userNick) splitNames)

       learnAllHosts x = foldl' (flip learnUserInfo) x (map fst splitNames)

-- | Compute the nickname and channel modes from an entry in
-- a NAMES reply. The leading channel prefixes are translated
-- into the appropriate modes.
splitNamesReplyName ::
  [(Char,Char)]        {- ^ [(mode,prefix)]   -} ->
  ByteString           {- ^ names entry       -} ->
  (UserInfo, String) {- ^ (nickname, modes) -}
splitNamesReplyName modeMap = aux []
  where
  aux modes n =
    case B8.uncons n of
      Just (x,xs)
        | Just (mode,_) <- find (\(_mode,symbol) -> x == symbol) modeMap
        -> aux (mode:modes) xs

      _ -> (parseUserInfo n,modes)

------------------------------------------------------------------------
-- Type describing computations that will require zero or more messages
-- to perform complex updates to the model.
------------------------------------------------------------------------

-- | Execute the 'Logic' value using a given operation for sending and
-- recieving IRC messages.
runLogic :: UTCTime -> Logic a -> Free LogicOp (Either String a)
runLogic now (Logic f) = runErrorT (runReaderT f now)

data LogicOp r
  = Expect  (MsgFromServer -> r)
  | Emit ByteString r
  | Record Identifier IrcMessage r
  deriving (Functor)

newtype Logic a = Logic (ReaderT UTCTime (ErrorT String (Free LogicOp)) a)
  deriving (Functor, Applicative, Monad)

getStamp :: Logic UTCTime
getStamp = Logic ask

recordFor :: Identifier -> IrcMessage -> Logic ()
recordFor target msg = Logic (wrap (Record target msg (return ())))

getMessage :: Logic MsgFromServer
getMessage = Logic (wrap (Expect return))

sendMessage :: ByteString -> Logic ()
sendMessage x = Logic (wrap (Emit x (return ())))


-- | Add a message to the client state for users and channels.
-- The user or channel record should be added if it does not already
-- exist.
recordMessage ::
  IrcMessage ->
  Identifier {- ^ target -} ->
  IrcConnection ->
  Logic IrcConnection
recordMessage mesg target conn =
  do let mesg1 = set mesgMe isMe mesg
     recordFor target mesg1
     return conn

  where
  isMe = isMyNick (views mesgSender userNick mesg) conn

-- | Predicate to determine if a given identifier is the primary nick
-- for the given connection.
isMyNick :: Identifier -> IrcConnection -> Bool
isMyNick nick conn = nick == view connNick conn

-- | Predicate for identifiers to identify which represent channel names.
-- Channel prefixes are configurable, but the most common is @#@
isChannelName :: Identifier -> IrcConnection -> Bool
isChannelName c conn =
  case B8.uncons (idBytes c) of
    Just (x,xs) -> not (B.null xs)
                && x `elem` view connChanTypes conn
    _ -> False -- probably shouldn't happen

-- | Predicate for identifiers to identify which represent nicknames
isNickName :: Identifier -> IrcConnection -> Bool
isNickName c conn =
  case B8.uncons (idBytes c) of
    Just (x,_) -> not (x `elem` view connChanTypes conn)
    _ -> False -- probably shouldn't happen

splitStatusMsg :: Identifier -> IrcConnection -> (String,Identifier)
splitStatusMsg target conn = aux [] (idBytes target)
  where
  aux acc bs =
    case B8.uncons bs of
      Just (x,xs) | x `elem` view connStatusMsg conn -> aux (x:acc) xs
      _ -> (reverse acc, mkId bs)
