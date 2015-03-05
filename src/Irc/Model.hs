{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
module Irc.Model where

import Data.Map (Map)
import Data.Monoid
import Data.Char (chr)
import Data.Text (Text)
import Control.Applicative
import Control.Lens
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.List (foldl',find,nub,delete,intersect)
import Data.Time
import Data.Word
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import Irc.Format
import Irc.Cmd
import Irc.Core
import Irc.Core.Prisms
import Irc.List (List)
import qualified Irc.List as List

data IrcConnection = IrcConnection
  { _connNick     :: Identifier
  , _connChannels :: !(Map Identifier IrcChannel)
  , _connMessages :: !(List IrcMessage)
  , _connId       :: Maybe ByteString
  , _connChanTypes :: [Word8]
  , _connUsers    :: !(Map Identifier IrcUser)
  , _connChanModes :: IrcChanModes
  , _connMyInfo   :: Maybe (ByteString,ByteString)
  , _connSasl     :: Maybe (ByteString,ByteString)
  }
  deriving (Read, Show)

defaultIrcConnection :: IrcConnection
defaultIrcConnection = IrcConnection
  { _connNick      = mkId ""
  , _connChannels  = mempty
  , _connMessages  = mempty
  , _connId        = Nothing
  , _connChanTypes = map (fromIntegral.ord) "#" -- TODO: Use ISupport
  , _connUsers     = mempty
  , _connChanModes = defaultChanModes -- TODO: Use ISupport
  , _connMyInfo    = Nothing
  , _connSasl      = Nothing
  }

data IrcChanModes = IrcChanModes
  { _modesLists :: String
  , _modesAlwaysArg :: String
  , _modesSetArg :: String
  , _modesNeverArg :: String
  , _modesPrefixModes :: [(Char,Char)]
  }
  deriving (Read, Show)

defaultChanModes :: IrcChanModes
defaultChanModes = IrcChanModes
  { _modesLists     = "eIbq"
  , _modesAlwaysArg = "k"
  , _modesSetArg    = "flj"
  , _modesNeverArg  = "CFLMPQScgimnprstz"
  , _modesPrefixModes = [('o','@'),('v','+')]
  }

data IrcChannel = IrcChannel
  { _chanTopic :: Maybe (Maybe (Text, ByteString, UTCTime)) -- TODO: use UserInfo
  , _chanUsers :: !(Map Identifier String) -- modes: ov
  , _chanModes :: Maybe (Map Char ByteString)
  , _chanCreation :: Maybe UTCTime
  , _chanMessages :: List IrcMessage
  , _chanMaskLists :: Map Char [IrcMaskEntry]
  , _chanUrl :: Maybe ByteString
  }
  deriving (Read, Show)

defaultChannel :: IrcChannel
defaultChannel = IrcChannel
  { _chanTopic = Nothing
  , _chanModes = Nothing
  , _chanCreation = Nothing
  , _chanUsers = mempty
  , _chanMessages = mempty
  , _chanMaskLists = mempty
  , _chanUrl = Nothing
  }

data IrcMaskEntry = IrcMaskEntry
  { _maskEntryMask  :: ByteString
  , _maskEntryWho   :: ByteString
  , _maskEntryStamp :: UTCTime
  }
  deriving (Read, Show)

data IrcMessage = IrcMessage
  { _mesgType :: !IrcMessageType
  , _mesgSender :: !UserInfo
  , _mesgStamp :: !UTCTime
  , _mesgMe :: !Bool
  , _mesgModes :: String
  }
  deriving (Read, Show)

data IrcMessageType
  = PrivMsgType   Text
  | NoticeMsgType Text
  | ActionMsgType Text
  | JoinMsgType
  | KickMsgType   Identifier Text
  | PartMsgType   Text
  | QuitMsgType   Text
  | NickMsgType   Identifier
  | TopicMsgType  Text
  | ErrorMsgType  Text
  | ModeMsgType Bool Char ByteString
  deriving (Read, Show)

data IrcUser = IrcUser
  { _usrAway :: !Bool
  , _usrMessages :: !(List IrcMessage)
  }
  deriving (Read,Show)

makeLenses ''IrcConnection
makeLenses ''IrcChannel
makeLenses ''IrcMessage
makeLenses ''IrcUser
makeLenses ''IrcMaskEntry
makeLenses ''IrcChanModes


-- | Primary state machine step function. Call this function with a timestamp
-- and a server message to update the 'IrcConnection' state. If additional
-- messages are required they will be requested via the 'Logic' type.
advanceModel :: MsgFromServer -> IrcConnection -> Logic IrcConnection
advanceModel msg0 conn =
  do stamp <- getStamp
     case msg0 of

       Ping x -> sendMessage (pongCmd x) >> return conn

       RplWelcome  txt -> doServerMessage "Welcome" txt conn
       RplYourHost txt -> doServerMessage "YourHost" txt conn
       RplCreated  txt -> doServerMessage "Created" txt conn
       RplMyInfo host version _ _ _ ->
         return (set connMyInfo (Just (host,version)) conn)

       RplLuserOp _         -> return conn
       RplLuserChannels _   -> return conn
       RplLuserMe _         -> return conn
       RplLuserClient _     -> return conn
       RplLocalUsers _ _    -> return conn
       RplGlobalUsers _ _   -> return conn
       RplStatsConn _       -> return conn


       Join who chan
         | isMyNick (userNick who) conn -> doAddChannel chan conn
         | otherwise -> doJoinChannel stamp who chan conn


       ExtJoin who chan _account _realname
         | isMyNick (userNick who) conn -> doAddChannel chan conn
         | otherwise -> doJoinChannel stamp who chan conn

       Part who chan reason
         | isMyNick (userNick who) conn ->
            return (set (connChannelAt chan) Nothing conn)
         | otherwise -> doPart stamp who chan reason conn

       Kick who chan tgt reason
         | isMyNick tgt conn ->
            return (set (connChannelAt chan) Nothing conn)
         | otherwise -> doKick stamp who chan tgt reason conn

       Quit who reason -> doQuit stamp who reason conn

       Nick who newnick
         | isMyNick (userNick who) conn -> doNick stamp who newnick
                                         $ set connNick newnick conn
         | otherwise -> doNick stamp who newnick conn

       RplChannelUrl chan url ->
            return (set (connChannelIx chan . chanUrl)
                        (Just url)
                        conn)

       RplNoTopicSet chan ->
          return (set (connChannelIx chan . chanTopic)
                      (Just Nothing)
                      conn)

       RplTopic chan topic ->
         do RplTopicWhoTime _ who time <- getMessage
            return (set (connChannelIx chan . chanTopic)
                        (Just (Just (asUtf8 topic,who,time)))
                        conn)

       Topic who chan topic ->
         return $ recordMessage m chan
                $ over (connChannelIx chan) changeTopic
                $ conn
         where
         changeTopic = set chanTopic
                           (Just (Just (topicText,userInfoBytestring who,stamp)))
         topicText = asUtf8 topic
         m = IrcMessage
                { _mesgType = TopicMsgType topicText
                , _mesgSender = who
                , _mesgStamp = stamp
                , _mesgModes = ""
                , _mesgMe = False
                }

       PrivMsg who chan msg -> return (recordMessage mesg chan conn)
         where
         mesg = IrcMessage
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

       Notice who chan msg -> doNotifyChannel stamp who chan msg conn

       RplYourId yourId -> return (set connId (Just yourId) conn)

       RplMotdStart -> return conn
       RplEndOfMotd -> return conn
       RplMotd x    -> doServerMessage "MOTD" x conn

       RplNameReply _ chan xs -> doNameReply chan xs conn

       RplChannelModeIs chan modes -> doChannelModeIs chan modes conn

       RplCreationTime chan creation ->
         return (set (connChannelIx chan . chanCreation) (Just creation) conn)

       RplWhoReply _chan _username _hostname _servername nickname flags _realname ->
         doWhoReply nickname flags conn

       RplEndOfWho _chan -> return conn

       RplIsOn nicks -> return (doIsOn nicks conn)

       RplBanList chan mask who when ->
         doMaskList (preview _RplBanList) (has _RplEndOfBanList) 'b' chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfBanList chan ->
         return (set (connChannelIx chan . chanMaskLists . at 'b') (Just []) conn)

       RplInviteList chan mask who when ->
         doMaskList (preview _RplInviteList) (has _RplEndOfInviteList) 'I' chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfInviteList chan ->
         return (set (connChannelIx chan . chanMaskLists . at 'I') (Just []) conn)

       RplExceptionList chan mask who when ->
         doMaskList (preview _RplExceptionList) (has _RplEndOfExceptionList) 'e' chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfExceptionList chan ->
         return (set (connChannelIx chan . chanMaskLists . at 'e') (Just []) conn)

       RplQuietList chan mode mask who when ->
         let fixup (a,_,c,d,e) = (a,c,d,e) in -- drop the matched mode field
         doMaskList (preview (_RplQuietList . to fixup)) (has _RplEndOfQuietList)
                    mode chan
                    [IrcMaskEntry
                      { _maskEntryMask  = mask
                      , _maskEntryWho   = who
                      , _maskEntryStamp = when
                      } ] conn

       RplEndOfQuietList chan mode ->
         return (set (connChannelIx chan . chanMaskLists . at mode) (Just []) conn)

       Mode who target (modes:args) ->
         return (doModeChange who stamp target modes args conn)

       ErrNoSuchNick nick ->
         doServerError ("No such nickname: " <> asUtf8 (idBytes nick)) conn
       ErrNoSuchChannel chan ->
         doServerError ("No such channel: " <> asUtf8 (idBytes chan)) conn
       ErrNoSuchService serv ->
         doServerError ("No such service: " <> asUtf8 (idBytes serv)) conn
       ErrNoSuchServer server ->
         doServerError ("No such server: " <> asUtf8 server) conn
       ErrBannedFromChan chan ->
         doServerError ("Cannot join " <> asUtf8 (idBytes chan) <> ", you are banned.") conn
       ErrBadChannelKey chan ->
         doServerError ("Cannot join " <> asUtf8 (idBytes chan) <> ", incorrect key.") conn
       ErrUnknownUmodeFlag ->
         doServerError "Unknown user mode" conn
       ErrUnknownMode mode ->
         doServerError ("Unknown mode: " <> Text.pack [mode]) conn
       ErrChannelFull chan ->
         doServerError ("Channel is full: " <> asUtf8 (idBytes chan)) conn
       ErrInviteOnlyChan chan ->
         doServerError ("Invite only channel: " <> asUtf8 (idBytes chan)) conn
       ErrWasNoSuchNick nick ->
         doServerError ("Was no nick: " <> asUtf8 (idBytes nick)) conn

       ErrChanOpPrivsNeeded chan ->
         return (recordMessage mesg chan conn)
         where
         mesg = IrcMessage
           { _mesgType    = ErrorMsgType "Channel privileges needed"
           , _mesgSender  = UserInfo (mkId "server") Nothing Nothing
           , _mesgStamp   = stamp
           , _mesgMe      = False
           , _mesgModes   = ""
           }

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
       RplEndOfWhois _nick ->
         doServerMessage "WHOIS" "--END--" conn

       RplWhoWasUser nick user host real ->
         doServerMessage "WHOWAS" (B8.unwords [idBytes nick, user, host, real]) conn
       RplEndOfWhoWas _nick ->
         doServerMessage "WHOWAS" "--END--" conn

       Cap "LS" caps -> doCapLs caps conn
       Cap "ACK" caps -> doCapAck caps conn
       Cap "NACK" caps -> sendMessage capEndCmd >> return conn
       RplSaslAborted -> return conn

       _ -> fail ("Unsupported: " ++ show msg0)

doCapLs :: ByteString -> IrcConnection -> Logic IrcConnection
doCapLs rawCaps conn =
  do sendMessage (capReqCmd activeCaps)
     return conn
  where
  activeCaps = intersect supportedCaps offeredCaps
  offeredCaps = B8.words rawCaps
  supportedCaps = ["multi-prefix"] ++ saslSupport
  saslSupport =
    case view connSasl conn of
      Nothing -> []
      Just{}  -> ["sasl"]

doCapAck :: ByteString -> IrcConnection -> Logic IrcConnection
doCapAck rawCaps conn =
  do let ackCaps = B8.words rawCaps

     conn' <- case view connSasl conn of
                Just (user,pass) | "sasl" `elem` ackCaps -> doSasl user pass conn
                _ -> return conn

     sendMessage capEndCmd
     return conn'

doSasl :: ByteString -> ByteString -> IrcConnection -> Logic IrcConnection
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


encodePlainAuthentication ::
  ByteString {- ^ username -} ->
  ByteString {- ^ password -} ->
  ByteString
encodePlainAuthentication user pass
  = Base64.encode
  $ B8.intercalate "\0" [user,user,pass]

doChannelModeIs :: Identifier -> [ByteString] -> IrcConnection -> Logic IrcConnection
doChannelModeIs chan []           conn =
  return (set (connChannelIx chan . chanModes) (Just mempty ) conn)
doChannelModeIs chan (modes:args) conn =
  case splitModes (view connChanModes conn) modes args of
    Nothing -> fail "Bad modeis string"
    Just xs -> return (set (connChannelIx chan . chanModes) (Just modeMap) conn)
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
     return (over connMessages (cons mesg) conn)

-- | Mark all the given nicks as active (not-away).
doIsOn ::
  [Identifier] {- ^ active nicks -} ->
  IrcConnection -> IrcConnection
doIsOn nicks conn = foldl' setIsOn conn nicks
  where
  setIsOn connAcc nick = set (connUserIx nick.usrAway) False connAcc

doModeChange ::
  UserInfo     {- ^ who           -} ->
  UTCTime      {- ^ when          -} ->
  Identifier   {- ^ target        -} ->
  ByteString   {- ^ modes changed -} ->
  [ByteString] {- ^ arguments     -} ->
  IrcConnection -> IrcConnection
doModeChange who now target modes0 args0 conn
  | isChannelName target conn =
      case splitModes modeSettings modes0 args0 of
        Nothing -> conn -- TODO: User modes
        Just ms -> doChannelModeChanges ms now who target conn

  -- TODO: Implement user modes
  | otherwise = conn

  where
  modeSettings = view connChanModes conn

doChannelModeChanges ::
  [(Bool, Char, ByteString)] {- ^ [(+/-,mode,argument)] -} ->
  UTCTime                    {- ^ timestamp             -} ->
  UserInfo                   {- ^ changer               -} ->
  Identifier                 {- ^ channel               -} ->
  IrcConnection -> IrcConnection
doChannelModeChanges ms now who chan conn0 = foldl aux conn0 ms
  where
  settings = view connChanModes conn0

  aux conn (polarity,m,a)
    = over (connChannelIx chan)
           (installModeChange settings now who chan polarity m a)
    $ recordMessage (modeMsg polarity m a) chan conn

  modeMsg polarity m arg = IrcMessage
         { _mesgType   = ModeMsgType polarity m arg
         , _mesgSender = who
         , _mesgStamp  = now
         , _mesgModes  = ""
         , _mesgMe     = False
         }

installModeChange ::
  IrcChanModes {- ^ settings -} ->
  UTCTime    {- ^ timestamp -} ->
  UserInfo   {- ^ changer -} ->
  Identifier {- ^ channel -} ->
  Bool       {- ^ +/-     -} ->
  Char       {- ^ mode    -} ->
  ByteString {- ^ argument -} ->
  IrcChannel -> IrcChannel
installModeChange settings now who chan polarity mode arg


  -- Handle bans, exceptions, invex, quiets
  | mode `elem` view modesLists settings =
      if polarity
         then over (chanMaskLists . ix mode)
                   (cons (IrcMaskEntry arg (userInfoBytestring who) now))

         else over (chanMaskLists . ix mode)
                   (filter (\x -> CI.foldCase (view maskEntryMask x)
                               /= CI.foldCase arg))

  -- Handle ops and voices
  | mode `elem` views modesPrefixModes (map fst) settings =
      if polarity
         then over (chanUserIx (mkId arg))
                   (nub . cons mode)

         else over (chanUserIx (mkId arg))
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
  IrcChanModes {- ^ mode interpretation -} ->
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

      _ | not polarity && m `elem` view modesSetArg icm
       ||                 m `elem` view modesNeverArg icm ->
              fmap (cons (polarity,m,"")) (rec polarity args)

        | otherwise -> Nothing

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
           return (set (connChannelIx chan . chanMaskLists . at mode)
                       (Just (reverse acc))
                       conn)

       _ -> fail "Expected mode list end"


-- | Update an 'IrcConnection' when a user changes nicknames.
doNick ::
  UTCTime    {- ^ timestamp           -} ->
  UserInfo   {- ^ old user infomation -} ->
  Identifier {- ^ new nickname        -} ->
  IrcConnection -> Logic IrcConnection
doNick stamp who newnick = return
                   . over connUsers updateUsers
                   . over (connChannels . mapped) updateChannel
  where
  oldnick = userNick who

  updateUsers :: Map Identifier IrcUser -> Map Identifier IrcUser
  updateUsers users
     = set (at oldnick) Nothing
     $ set (at newnick)
           (view (at oldnick) users) users

  updateChannel :: IrcChannel -> IrcChannel
  updateChannel chan
    | has (chanUserIx oldnick) chan
     = set (chanUserAt oldnick) Nothing
     $ set (chanUserAt newnick) (view (chanUserAt oldnick) chan)
     $ over chanMessages (cons m)
     $ chan
    | otherwise = chan

  m = IrcMessage
         { _mesgType = NickMsgType newnick
         , _mesgSender = who
         , _mesgStamp = stamp
         , _mesgModes = ""
         , _mesgMe = False
         }

-- | Update the 'IrcConnection' when a user parts from a channel.
doPart ::
  UTCTime    {- ^ timestamp        -} ->
  UserInfo   {- ^ user information -} ->
  Identifier {- ^ channel          -} ->
  ByteString {- ^ part reason      -} ->
  IrcConnection -> Logic IrcConnection
doPart stamp who chan reason conn =
   return $ over (connChannelIx chan) removeUser
          $ recordMessage mesg chan conn
  where
  mesg = IrcMessage
         { _mesgType = PartMsgType (asUtf8 reason)
         , _mesgSender = who
         , _mesgStamp = stamp
         , _mesgMe = False
         , _mesgModes = ""
         }

  removeUser = set (chanUserAt (userNick who)) Nothing

-- | Update an 'IrcConnection' when a user is kicked from a channel.
doKick ::
  UTCTime    {- ^ timestamp   -} ->
  UserInfo   {- ^ kicker      -} ->
  Identifier {- ^ channel     -} ->
  Identifier {- ^ kicked      -} ->
  ByteString {- ^ kick reason -} ->
  IrcConnection -> Logic IrcConnection
doKick stamp who chan tgt reason conn =
  return (recordMessage mesg chan conn)
  where
  mesg = IrcMessage
         { _mesgType = KickMsgType tgt (asUtf8 reason)
         , _mesgSender = who
         , _mesgStamp = stamp
         , _mesgModes = ""
         , _mesgMe = False
         }


doWhoReply :: Identifier -> ByteString -> IrcConnection -> Logic IrcConnection
doWhoReply nickname flags conn =
  return (set (connUserAt nickname) (Just $! u) conn)
  where
  away = not (B.null flags) && B.take 1 flags == "G"
  u = IrcUser
        { _usrAway     = away
        , _usrMessages = oldMessages
        }
  oldMessages =
    case preview (connUserIx nickname . usrMessages) conn of
      Nothing -> mempty
      Just xs -> xs

-- | Update an 'IrcConnection' with the quitting of a user.
doQuit ::
  UTCTime    {- ^ timestamp   -} ->
  UserInfo   {- ^ user info   -} ->
  ByteString {- ^ quit reason -} ->
  IrcConnection -> Logic IrcConnection
doQuit stamp who reason conn = return
                       $ over (connChannels . mapped) upd
                       $ set (connUserAt (userNick who)) Nothing
                       $ conn
  where
  mesg = IrcMessage
         { _mesgType = QuitMsgType (asUtf8 reason)
         , _mesgSender = who
         , _mesgStamp = stamp
         , _mesgMe = False
         , _mesgModes = ""
         }

  -- special case update because it affects all channels the user is in
  upd chan
    | has (chanUserIx (userNick who)) chan
          = over chanMessages (cons mesg)
          . set (chanUserAt (userNick who)) Nothing
          $ chan
    | otherwise = chan

doWho :: IrcConnection -> Logic IrcConnection
doWho conn =
  do msg <- getMessage
     case msg of
       RplWhoReply {} -> doWho conn
       RplEndOfWho {} -> return conn
       _ -> fail ("Expected who reply: " ++ show msg)

doAddChannel :: Identifier -> IrcConnection -> Logic IrcConnection
doAddChannel chan conn
  = return
  $ set (connChannelAt chan) (Just channel)
  $ conn
  where
  channel =
    set (chanUserAt (view connNick conn)) (Just "") defaultChannel

doJoinChannel :: UTCTime -> UserInfo -> Identifier -> IrcConnection -> Logic IrcConnection
doJoinChannel stamp who chan conn =
  return $ recordMessage m chan
         $ set (connChannelIx chan . chanUserAt (userNick who)) (Just "")
         $ conn
  where
  m = IrcMessage
        { _mesgType = JoinMsgType
        , _mesgSender = who
        , _mesgStamp = stamp
        , _mesgMe = False
        , _mesgModes = ""
        }

doNotifyChannel ::
  UTCTime ->
  UserInfo ->
  Identifier ->
  ByteString ->
  IrcConnection ->
  Logic IrcConnection
doNotifyChannel stamp who chan msg conn = return (recordMessage mesg chan conn)
  where
  mesg = IrcMessage
        { _mesgType = NoticeMsgType (asUtf8 msg)
        , _mesgSender = who
        , _mesgStamp = stamp
        , _mesgMe = False
        , _mesgModes = ""
        }

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
     return (over connMessages (cons m) conn)

doNameReply :: Identifier -> [ByteString] -> IrcConnection -> Logic IrcConnection
doNameReply chan xs conn =
  do msg <- getMessage
     case msg of
       RplNameReply _ _ x -> doNameReply chan (x++xs) conn
       RplEndOfNames _ -> return
                     $ set (connChannelIx chan . chanUsers)
                           users
                           conn
       _ -> fail "Expected end of names"
       where
       modeMap = view (connChanModes . modesPrefixModes) conn
       users = Map.fromList
             $ map (splitNamesReplyName modeMap) xs

-- | Compute the nickname and channel modes from an entry in
-- a NAMES reply. The leading channel prefixes are translated
-- into the appropriate modes.
splitNamesReplyName ::
  [(Char,Char)]        {- ^ [(mode,prefix)]   -} ->
  ByteString           {- ^ names entry       -} ->
  (Identifier, String) {- ^ (nickname, modes) -}
splitNamesReplyName modeMap = aux []
  where
  aux modes n =
    case B8.uncons n of
      Just (x,xs)
        | Just (mode,_) <- find (\(_mode,symbol) -> x == symbol) modeMap
        -> aux (mode:modes) xs

      _                 -> (mkId n,modes)

------------------------------------------------------------------------
-- Type describing computations that will require zero or more messages
-- to perform complex updates to the model.
------------------------------------------------------------------------

-- | Execute the 'Logic' value using a given operation for sending and
-- recieving IRC messages.
runLogic ::
  Monad m =>
  UTCTime ->
  m MsgFromServer {- ^ operation for receiving message -} ->
  (ByteString -> m ()) {- ^ operation for sending message -} ->
  Logic a ->
  m (Either String a)
runLogic now getMsg sendMsg (Logic f) = iter interp (fmap (return . Right) (runReaderT f now))
  where
  interp (Expect k)  = k =<< getMsg
  interp (Failure e) = return (Left e)
  interp (Emit x m) = sendMsg x >> m

data LogicOp r
  = Expect  (MsgFromServer -> r)
  | Emit ByteString r
  | Failure String
  deriving (Functor)

newtype Logic a = Logic { unLogic :: ReaderT UTCTime (Free LogicOp) a }
  deriving (Functor, Applicative)

instance Monad Logic where
  fail          = Logic . wrap . Failure
  m >>= f       = Logic (unLogic . f =<< unLogic m)
  return        = Logic . return

getStamp :: Logic UTCTime
getStamp = Logic ask

getMessage :: Logic MsgFromServer
getMessage = Logic (wrap (Expect return))

sendMessage :: ByteString -> Logic ()
sendMessage x = Logic (wrap (Emit x (return ())))

asUtf8 :: ByteString -> Text
asUtf8 = Text.decodeUtf8With Text.lenientDecode

userInfoBytestring :: UserInfo -> ByteString
userInfoBytestring u = idBytes (userNick u)
                    <> maybe B.empty ("!" <>) (userName u)
                    <> maybe B.empty ("@" <>) (userHost u)

activeChannelNames :: IrcConnection -> [Identifier]
activeChannelNames = Map.keys . view connChannels

activeUserNames :: IrcConnection -> [Identifier]
activeUserNames = Map.keys . Map.filter isActive . view connUsers
  where
  isActive x = views usrMessages List.length x > 0

connChannelAt :: Functor f => Identifier -> LensLike' f IrcConnection (Maybe IrcChannel)
connChannelAt k = connChannels . at k

connChannelIx :: Applicative f => Identifier -> LensLike' f IrcConnection IrcChannel
connChannelIx k = connChannels . ix k

connUserAt :: Applicative f => Identifier -> LensLike' f IrcConnection (Maybe IrcUser)
connUserAt k = connUsers . at k

connUserIx :: Applicative f => Identifier -> LensLike' f IrcConnection IrcUser
connUserIx k = connUsers . ix k

chanUserAt :: Applicative f => Identifier -> LensLike' f IrcChannel (Maybe String)
chanUserAt k = chanUsers . at k

chanUserIx :: Applicative f => Identifier -> LensLike' f IrcChannel String
chanUserIx k = chanUsers . ix k




-- | Add a message to the client state for users and channels.
-- The user or channel record should be added if it does not already
-- exist.
recordMessage ::
  IrcMessage ->
  Identifier {- ^ target -} ->
  IrcConnection ->
  IrcConnection
recordMessage mesg target conn =
  case B.uncons (idBytes target) of
    Nothing -> error "recordMessage: empty target"
    Just (t,_)
      | t `elem` view connChanTypes conn -> recordChannelMsg conn
      | otherwise                        -> recordUserMsg conn

  where
  isMe = isMyNick (views mesgSender userNick mesg) conn

  mesg1 = set mesgMe isMe mesg

  -- Messages aren't recorded if we aren't in the channel
  recordChannelMsg =
    over (connChannelIx target) $ \channel ->

      let mesg' = set mesgModes
                      (view (chanUserIx (views mesgSender userNick mesg)) channel)
                      mesg1

      in over chanMessages (cons mesg') channel

  recordUserMsg
    | isMe      = recordUserMsgFor target
    | otherwise = recordUserMsgFor (userNick (view mesgSender mesg))

  recordUserMsgFor u =
    over (connUserAt u) $ \mbUser ->
      case mbUser of
        Nothing -> Just $
          IrcUser
            { _usrAway = False
            , _usrMessages = List.fromList [mesg1]
            }
        Just usr -> Just (over usrMessages (cons mesg1) usr)

isMyNick :: Identifier -> IrcConnection -> Bool
isMyNick nick conn = nick == view connNick conn

isChannelName :: Identifier -> IrcConnection -> Bool
isChannelName c conn =
  case B.uncons (idBytes c) of
    Just (x,_) -> x `elem` view connChanTypes conn
    _ -> False -- probably shouldn't happen

