{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}


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

  -- * High-level IRC events
  , IrcMessage(..)
  , IrcMessageType(..)
  , mesgType
  , mesgSender
  , mesgStamp
  , mesgMe
  , mesgModes

  -- * User metadata
  , IrcUser(..)
  , usrAway
  , usrAccount
  , defaultIrcUser

  -- * Model execution
  , runLogic
  , LogicOp(..)
  , Logic

  -- * General functionality
  , advanceModel
  , isChannelName
  , isMyNick
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Free
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.List (foldl',find,nub,delete,intersect)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import Data.Time
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as Text

import Irc.Format
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
  , _connChanTypes :: [Word8]
  , _connUsers    :: !(Map Identifier IrcUser)
  , _connChanModeTypes :: ModeTypes
  , _connUserModeTypes :: ModeTypes
  , _connMyInfo   :: Maybe (ByteString,ByteString)
  , _connSasl     :: Maybe (ByteString,ByteString)
  , _connUmode    :: ByteString
  , _connSnoMask  :: ByteString
  }
  deriving (Read, Show)

-- | 'IrcConnection' value with everything unspecified
defaultIrcConnection :: IrcConnection
defaultIrcConnection = IrcConnection
  { _connNick      = mkId ""
  , _connChannels  = mempty
  , _connId        = Nothing
  , _connChanTypes = map (fromIntegral.ord) "#" -- TODO: Use ISupport
  , _connUsers     = mempty
  , _connChanModeTypes = defaultChanModeTypes -- TODO: Use ISupport
  , _connUserModeTypes = defaultUmodeTypes
  , _connMyInfo    = Nothing
  , _connSasl      = Nothing
  , _connUmode     = ""
  , _connSnoMask   = ""
  }

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

-- | 'IrcMessage' represents a high-level event to be communicated out
-- to the library user when something changes on a connection.
data IrcMessage = IrcMessage
  { _mesgType :: !IrcMessageType
  , _mesgSender :: !UserInfo
  , _mesgStamp :: !UTCTime
  , _mesgMe :: !Bool
  , _mesgModes :: String
  }
  deriving (Read, Show)

-- | Event types and associated fields used by 'IrcMessage'.
data IrcMessageType
  = PrivMsgType   Text
  | NoticeMsgType Text
  | ActionMsgType Text
  | AwayMsgType   Text
  | JoinMsgType
  | KickMsgType   Identifier Text
  | PartMsgType   Text
  | QuitMsgType   Text
  | NickMsgType   Identifier
  | TopicMsgType  Text
  | ErrorMsgType  Text
  | ModeMsgType Bool Char ByteString
  deriving (Read, Show)

-- | 'IrcUser' is the type of user-level metadata tracked for
-- the users visible on the current IRC connection.
data IrcUser = IrcUser
  { _usrAway :: !Bool
  , _usrAccount :: Maybe ByteString
  }
  deriving (Read,Show)

-- | This represents the metadata of an unknown user.
defaultIrcUser :: IrcUser
defaultIrcUser = IrcUser
  { _usrAway    = False
  , _usrAccount = Nothing
  }

makeLenses ''IrcConnection
makeLenses ''IrcChannel
makeLenses ''IrcMessage
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

       RplWelcome  txt -> doServerMessage "Welcome" txt conn
       RplYourHost txt -> doServerMessage "YourHost" txt conn
       RplCreated  txt -> doServerMessage "Created" txt conn
       RplMyInfo host version _ _ _ ->
         return (set connMyInfo (Just (host,version)) conn)

       -- Random uninteresting statistics
       RplLuserOp _         -> return conn
       RplLuserChannels _   -> return conn
       RplLuserMe _         -> return conn
       RplLuserClient _     -> return conn
       RplLocalUsers _ _    -> return conn
       RplGlobalUsers _ _   -> return conn
       RplStatsConn _       -> return conn
       RplEndOfStats _      -> return conn
       RplLuserUnknown _    -> return conn
       RplLuserAdminMe _    -> return conn
       RplLuserAdminLoc1 _  -> return conn
       RplLuserAdminLoc2 _  -> return conn
       RplLuserAdminEmail _ -> return conn

       -- Channel list not implemented
       RplListStart     -> return conn
       RplList chan count topic -> doList chan count topic conn
       RplListEnd       -> return conn

       RplUserHost host ->
         doServerMessage "USERHOST" (B8.unwords host) conn

       RplTime _server time -> doServerMessage "TIME" time conn

       RplInfo _ -> return conn
       RplEndOfInfo -> return conn

       Join who chan -> doJoinChannel who Nothing chan conn
       ExtJoin who chan account _realname -> doJoinChannel who account chan conn
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
         return (set (connUsers . ix (userNick who) . usrAway) True conn)

       RplYourId yourId -> return (set connId (Just yourId) conn)

       RplMotdStart -> return conn
       RplEndOfMotd -> return conn
       RplMotd x    -> doServerMessage "MOTD" x conn

       RplNameReply _ chan xs -> doNameReply chan xs conn
       RplEndOfNames _ -> return conn

       RplChannelModeIs chan modes params -> doChannelModeIs chan modes params conn

       RplCreationTime chan creation ->
         return (set (connChannels . ix chan . chanCreation) (Just creation) conn)

       RplWhoReply _chan _username _hostname _servername nickname flags _realname ->
         doWhoReply nickname flags conn

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

       ErrNoSuchService serv ->
         doServerError ("No such service: " <> asUtf8 (idBytes serv)) conn
       ErrNoSuchServer server ->
         doServerError ("No such server: " <> asUtf8 server) conn
       ErrUnknownMode mode ->
         doServerError ("Unknown mode: " <> Text.pack [mode]) conn
       ErrNoPrivileges ->
         doServerError "No privileges" conn
       ErrUnknownUmodeFlag mode ->
         doServerError ("Unknown UMODE: " <> Text.pack [mode]) conn
       ErrUnknownCommand cmd ->
         doServerError ("Unknown command: " <> asUtf8 cmd) conn
       ErrNoTextToSend ->
         doServerError "No text to send" conn
       ErrNoMotd ->
         doServerError "No MOTD" conn
       ErrNoRecipient ->
         doServerError "No recipient" conn
       ErrNoAdminInfo ->
         doServerError "No admin info" conn
       ErrNickInUse ->
         doServerError "Nick in use" conn
       ErrNeedMoreParams cmd ->
         doServerError ("Need more parameters: " <> asUtf8 cmd) conn
       ErrAlreadyRegistered ->
         doServerError "Already registered" conn
       ErrNoPermForHost ->
         doServerError "No permission for host" conn
       ErrPasswordMismatch ->
         doServerError "Password mismatch" conn
       ErrUsersDontMatch ->
         doServerError "Can't change modes for other users" conn
       ErrHelpNotFound _ ->
         doServerError "Help topic not found" conn
       ErrBadChanName name ->
         doServerError ("Illegal channel name: " <> asUtf8 name) conn

       ErrNoSuchNick nick ->
         doChannelError nick "No such nick" conn
       ErrWasNoSuchNick nick ->
         doChannelError nick "Was no such nick" conn

       ErrTooManyChannels chan ->
         doChannelError chan "Too many channels joined" conn
       ErrUserNotInChannel nick chan ->
         doChannelError chan ("Not in channel: " <> asUtf8 (idBytes nick)) conn
       ErrNotOnChannel chan ->
         doChannelError chan "Must join channel" conn
       ErrChanOpPrivsNeeded chan ->
         doChannelError chan "Channel privileges needed" conn
       ErrBadChannelKey chan ->
         doChannelError chan "Bad channel key" conn
       ErrBadChannelMask chan ->
         doChannelError chan "Bad channel mask" conn
       ErrBannedFromChan chan ->
         doChannelError chan "Unable to join due to ban" conn
       ErrChannelFull chan ->
         doChannelError chan "Channel is full" conn
       ErrInviteOnlyChan chan ->
         doChannelError chan "Invite only channel" conn
       ErrNoSuchChannel chan ->
         doChannelError chan "No such channel" conn
       ErrCannotSendToChan chan ->
         doChannelError chan "Cannot send to channel" conn
       ErrTooManyTargets target ->
         doChannelError target "Too many targets" conn
       ErrBanListFull chan mode  ->
         doChannelError chan ("Ban list full: " <> Text.singleton mode) conn
       ErrUserOnChannel nick chan ->
         doChannelError chan ("User already on channel: " <> asUtf8 (idBytes nick)) conn
       ErrLinkChannel chanFrom chanTo ->
         doChannelError chanFrom ("Forwarded to: " <> asUtf8 (idBytes chanTo)) conn
       ErrNeedReggedNick chan ->
         doChannelError chan "Registered nick required" conn
       ErrVoiceNeeded chan ->
         doChannelError chan "Voice or operator status required" conn

       -- TODO: Make a message for this
       RplKnockDelivered chan ->
         doChannelError chan "Knock delivered" conn
       RplKnock chan user ->
         doChannelError chan ("Invite request: " <> asUtf8 (renderUserInfo user)) conn
       ErrKnockOnChan chan ->
         doChannelError chan "Attempted to knock joined channel" conn
       ErrTooManyKnocks chan ->
         doChannelError chan "Too many knocks" conn
       ErrChanOpen chan ->
         doChannelError chan "Knock failed, channel is open" conn
       RplInviting nick chan ->
         doChannelError chan ("Inviting " <> asUtf8 (idBytes nick)) conn
       Invite who chan ->
         doChannelError chan ("Invited by " <> renderUserInfo who <> " ") conn

       -- TODO: Structure this more nicely than as simple message,
       -- perhaps store it in the user map
       RplWhoisUser nick user host real ->
         doServerMessage "WHOIS" (B8.unwords [idBytes nick, user, host, real]) conn
       RplWhoisChannels _nick channels ->
         doServerMessage "WHOIS" channels conn
       RplWhoisServer _nick host txt ->
         doServerMessage "WHOIS" (B8.unwords [host,txt]) conn
       RplWhoisSecure _nick ->
         doServerMessage "WHOIS" "secure connection" conn
       RplWhoisHost _nick txt ->
         doServerMessage "WHOIS" txt conn
       RplWhoisIdle _nick idle _signon ->
         doServerMessage "WHOIS" ("Idle seconds: " <> idle) conn
       RplWhoisAccount _nick account ->
         doServerMessage "WHOIS" ("Logged in as: " <> account) conn
       RplWhoisModes _nick modes args ->
         doServerMessage "WHOIS" ("Modes: " <> B8.unwords (modes:args)) conn
       RplWhoisOperator _nick txt  ->
         doServerMessage "WHOIS" ("Operator: " <> txt) conn
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

       RplNickLocked ->
         doServerError "Nickname locked" conn
       RplLoggedIn account ->
         doServerMessage "LOGIN" account conn
       RplLoggedOut ->
         doServerMessage "LOGOUT" "" conn
       RplSaslSuccess ->
         doServerError "Unexpected SASL Success" conn
       RplSaslFail ->
         doServerError "Unexpected SASL Fail" conn
       RplSaslTooLong ->
         doServerError "Unexpected SASL Too Long" conn
       RplSaslAlready ->
         doServerError "Unexpected SASL Already" conn
       RplSaslMechs _ ->
         doServerError "Unexpected SASL Mechanism List" conn
       Authenticate _ ->
         doServerError "Unexpected Authenticate" conn

       Error e ->
         doServerError (asUtf8 e) conn

       RplISupport _ -> fail "Unsupported isupport"

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
     let mesg = IrcMessage
           { _mesgType    = AwayMsgType message
           , _mesgSender  = UserInfo nick Nothing Nothing
           , _mesgStamp   = stamp
           , _mesgMe      = False
           , _mesgModes   = ""
           }
     recordMessage mesg nick conn

doChannelError ::
  Identifier {- ^ channel -} ->
  Text       {- ^ error   -} ->
  IrcConnection -> Logic IrcConnection
doChannelError chan reason conn =
  do stamp <- getStamp
     let mesg = IrcMessage
           { _mesgType    = ErrorMsgType reason
           , _mesgSender  = UserInfo (mkId "server") Nothing Nothing
           , _mesgStamp   = stamp
           , _mesgMe      = False
           , _mesgModes   = ""
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
     let mesg = IrcMessage
                { _mesgType    = ty
                , _mesgSender  = who
                , _mesgStamp   = stamp
                , _mesgMe      = False
                , _mesgModes   = ""
                }
         ty
           | B.length msg >= 9
           , B.isPrefixOf "\SOHACTION " msg
           , B.last msg == 1
           = ActionMsgType (asUtf8 (B.init (B.drop 8 msg)))
           | otherwise = PrivMsgType (asUtf8 msg)

     recordMessage mesg chan conn

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
         m = IrcMessage
                { _mesgType = TopicMsgType topicText
                , _mesgSender = who
                , _mesgStamp = stamp
                , _mesgModes = ""
                , _mesgMe = False
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
                   "extended-join","multi-prefix"]
  saslSupport =
    case view connSasl conn of
      Nothing -> []
      Just{}  -> ["sasl"]

doCapAck ::
  ByteString {- ^ raw, spaces delimited caps list -} ->
  IrcConnection -> Logic IrcConnection
doCapAck rawCaps conn =
  do let ackCaps = B8.words rawCaps

     conn' <- case view connSasl conn of
                Just (user,pass) | "sasl" `elem` ackCaps -> doSasl user pass conn
                _ -> return conn

     sendMessage capEndCmd
     return conn'

-- | Complete SASL PLAIN authentication
doSasl ::
  ByteString {- ^ SASL authentication/authorization name -} ->
  ByteString {- ^ SASL password -} ->
  IrcConnection -> Logic IrcConnection
doSasl user pass conn =
  do sendMessage (authenticateCmd "PLAIN")
     Authenticate "+" <- getMessage

     sendMessage (authenticateCmd (encodePlainAuthentication user pass))
     resp1 <- getMessage
     case resp1 of
       RplLoggedIn _account ->
         do RplSaslSuccess <- getMessage
            doServerMessage "SASL" "Authentication successful" conn

       RplSaslFail ->
         do doServerMessage "SASL" "Authentication failed" conn

       _ -> fail "Bad SASL interaction"


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
     let mesg = IrcMessage
           { _mesgType    = ErrorMsgType err
           , _mesgSender  = UserInfo (mkId "server") Nothing Nothing
           , _mesgStamp   = stamp
           , _mesgMe      = False
           , _mesgModes   = ""
           }
     recordFor "" mesg
     return conn

-- | Mark all the given nicks as active (not-away).
doIsOn ::
  [Identifier] {- ^ active nicks -} ->
  IrcConnection -> IrcConnection
doIsOn nicks conn = foldl' setIsOn conn nicks
  where
  setIsOn connAcc nick = set (connUsers . ix nick.usrAway) False connAcc

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
    modeMsg = IrcMessage
           { _mesgType   = ModeMsgType polarity m a
           , _mesgSender = who
           , _mesgStamp  = now
           , _mesgModes  = ""
           , _mesgMe     = False
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
                   (filter (\x -> CI.foldCase (view maskEntryMask x)
                               /= CI.foldCase arg))

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

-- | Split up a mode change command and arguments into individual changes
-- given a configuration.
splitModes ::
  ModeTypes {- ^ mode interpretation -} ->
  ByteString   {- ^ modes               -} ->
  [ByteString] {- ^ arguments           -} ->
  Maybe [(Bool,Char,ByteString)]
splitModes icm modes0 =
  foldr aux (\_ _ -> Just []) (B8.unpack modes0) True
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
     let m = IrcMessage
                { _mesgType = NickMsgType newnick
                , _mesgSender = who
                , _mesgStamp = stamp
                , _mesgModes = ""
                , _mesgMe = False
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
     let mesg = IrcMessage
                { _mesgType = PartMsgType (asUtf8 reason)
                , _mesgSender = who
                , _mesgStamp = stamp
                , _mesgMe = False
                , _mesgModes = ""
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
     let mesg = IrcMessage
                { _mesgType = KickMsgType tgt (asUtf8 reason)
                , _mesgSender = who
                , _mesgStamp = stamp
                , _mesgModes = ""
                , _mesgMe = False
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


doWhoReply :: Identifier -> ByteString -> IrcConnection -> Logic IrcConnection
doWhoReply nickname flags conn =
  return (over (connUsers . ix nickname)
               (set usrAway away)
               conn)
  where
  away = not (B.null flags) && B.take 1 flags == "G"

-- | Update an 'IrcConnection' with the quitting of a user.
doQuit ::
  UserInfo   {- ^ user info   -} ->
  ByteString {- ^ quit reason -} ->
  IrcConnection -> Logic IrcConnection
doQuit who reason conn =
  do stamp <- getStamp
     let mesg = IrcMessage
                { _mesgType = QuitMsgType (asUtf8 reason)
                , _mesgSender = who
                , _mesgStamp = stamp
                , _mesgMe = False
                , _mesgModes = ""
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
  Maybe ByteString {- ^ account name -} ->
  Identifier       {- ^ channel      -} ->
  IrcConnection -> Logic IrcConnection
doJoinChannel who mbAcct chan conn =
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

     -- update user record
         conn3 = over (connUsers . at (userNick who))
                      (Just . set usrAccount mbAcct . fromMaybe defaultIrcUser)
                      conn2

     -- record join event
         m = IrcMessage
               { _mesgType = JoinMsgType
               , _mesgSender = who
               , _mesgStamp = stamp
               , _mesgMe = False
               , _mesgModes = ""
               }
     recordMessage m chan conn3

doNotifyChannel ::
  UserInfo ->
  Identifier ->
  ByteString ->
  IrcConnection ->
  Logic IrcConnection
doNotifyChannel who chan msg conn =
  do stamp <- getStamp
     let mesg = IrcMessage
               { _mesgType = NoticeMsgType (asUtf8 msg)
               , _mesgSender = who
               , _mesgStamp = stamp
               , _mesgMe = False
               , _mesgModes = ""
               }
     recordMessage mesg chan conn

doServerMessage ::
  ByteString {- ^ who -} ->
  ByteString {- ^ message -} ->
  IrcConnection -> Logic IrcConnection
doServerMessage who txt conn =
  do stamp <- getStamp
     let m = IrcMessage
               { _mesgType    = PrivMsgType (asUtf8 txt)
               , _mesgSender  = UserInfo (mkId who) Nothing Nothing
               , _mesgStamp   = stamp
               , _mesgMe      = False
               , _mesgModes   = ""
               }
     recordFor "" m
     return conn

doNameReply :: Identifier -> [ByteString] -> IrcConnection -> Logic IrcConnection
doNameReply chan xs conn =
  do msg <- getMessage
     case msg of
       RplNameReply _ _ x -> doNameReply chan (x++xs) conn
       RplEndOfNames _ -> return
                     $ set (connChannels . ix chan . chanUsers)
                           users
                           conn
       _ -> fail "Expected end of names"
       where
       modeMap = view (connChanModeTypes . modesPrefixModes) conn
       users = Map.fromList
             $ map (over _1 userNick) -- drop hostname for now
             $ map (splitNamesReplyName modeMap) xs
         -- TODO : Record userhost

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
  case B.uncons (idBytes c) of
    Just (x,_) -> x `elem` view connChanTypes conn
    _ -> False -- probably shouldn't happen

