{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Irc.Model where

import Data.Map (Map)
import Data.Monoid
import Data.Char (chr)
import Data.Text (Text)
import Control.Applicative
import Control.Lens
import Control.Monad.Free
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.List (find,nub,delete)
import Data.Time
import Data.Word
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import Irc.Format
import Irc.Core
import Irc.List (List)
import qualified Irc.List as List

data IrcConnection = IrcConnection
  { _connNick     :: ByteString
  , _connChannels :: !(Map (CI ByteString) IrcChannel)
  , _connMotd     :: Maybe [ByteString]
  , _connId       :: Maybe ByteString
  , _connChanTypes :: [Word8]
  , _connUsers    :: !(Map (CI ByteString) IrcUser)
  , _connChanModes :: IrcChanModes
  , _connMyInfo   :: Maybe (ByteString,ByteString)
  }
  deriving (Read, Show)

defaultIrcConnection :: IrcConnection
defaultIrcConnection = IrcConnection
  { _connNick     = ""
  , _connChannels = Map.empty
  , _connMotd     = Nothing
  , _connId       = Nothing
  , _connChanTypes = map (fromIntegral.ord) "#" -- TODO: Use ISupport
  , _connUsers = Map.empty
  , _connChanModes = defaultChanModes -- TODO: Use ISupport
  , _connMyInfo   = Nothing
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
  , _chanUsers :: !(Map (CI ByteString) String) -- modes: ov
  , _chanModes :: Maybe [ByteString]
  , _chanCreation :: Maybe UTCTime
  , _chanMessages :: List IrcMessage
  , _chanBans :: Maybe [IrcBan]
  , _chanUrl :: Maybe ByteString
  }
  deriving (Read, Show)

defaultChannel :: IrcChannel
defaultChannel = IrcChannel
  { _chanTopic = Nothing
  , _chanModes = Nothing
  , _chanCreation = Nothing
  , _chanUsers = Map.empty
  , _chanMessages = mempty
  , _chanBans = Nothing
  , _chanUrl = Nothing
  }

data IrcBan = IrcBan
  { _banBannee :: ByteString
  , _banBanner :: ByteString
  , _banStamp  :: UTCTime
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
  | KickMsgType   ByteString Text
  | PartMsgType   Text
  | QuitMsgType   Text
  | NickMsgType   ByteString
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
makeLenses ''IrcBan
makeLenses ''IrcChanModes



advanceModel :: UTCTime -> MsgFromServer -> IrcConnection -> Logic IrcConnection
advanceModel stamp msg0 conn =
  case msg0 of

       RplWelcome  _ -> return conn
       RplYourHost _ -> return conn
       RplCreated  _ -> return conn

       RplMyInfo host version _ _ _ ->
         return (set connMyInfo (Just (host,version)) conn)

       RplLuserOp _ _       -> return conn
       RplLuserChannels _ _ -> return conn
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
         | isMyNick (userNick who) conn -> return (set connNick newnick conn)
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

       RplMotdStart -> doMotd [] conn

       RplNameReply _ chan xs -> doNameReply chan xs conn

       RplChannelModeIs chan modes ->
         return (set (connChannelIx chan . chanModes) (Just modes) conn)

       RplCreationTime chan creation ->
         return (set (connChannelIx chan . chanCreation) (Just creation) conn)

       RplWhoReply _chan _username _hostname _servername nickname flags _realname ->
         doWhoReply nickname flags conn

       RplEndOfWho _chan -> return conn

       RplIsOn nicks -> return (foldl (\connAcc nick -> set (connUserIx nick.usrAway) False connAcc)
                                      conn
                                      nicks)

       RplBanList _chan bannee banner bantime ->
         doBanList [IrcBan { _banBannee = bannee
                           , _banBanner = banner
                           , _banStamp  = bantime
                           } ] conn

       RplEndOfBanList chan ->
         return (set (connChannelIx chan . chanBans) Nothing conn)

       Mode who target (modes:args) ->
         return (doModeChange who stamp target modes args conn)

       ErrChanOpPrivsNeeded chan txt ->
         return (recordMessage mesg chan conn)
         where
         mesg = IrcMessage
           { _mesgType    = ErrorMsgType (asUtf8 txt)
           , _mesgSender  = UserInfo "server" Nothing Nothing
           , _mesgStamp   = stamp
           , _mesgMe      = False
           , _mesgModes   = ""
           }

       _ -> fail ("Unsupported: " ++ show msg0)


doModeChange :: UserInfo -> UTCTime -> ByteString -> ByteString -> [ByteString] -> IrcConnection -> IrcConnection
doModeChange who now target modes0 args0 conn0 = aux True modes0 args0 conn0
  where
  modeSettings = view connChanModes conn0

  aux polarity modes args conn =
    case BS8.uncons modes of
      Nothing     -> conn
      Just ('+',ms) -> aux True  ms args conn
      Just ('-',ms) -> aux False ms args conn
      Just (m,ms)
        | m `elem` view modesLists     modeSettings ->
              aux polarity ms (drop 1 args) conn -- TODO: Manage lists
        | m `elem` view modesAlwaysArg modeSettings -> aux polarity ms (drop 1 args) conn
        | m `elem` view modesSetArg    modeSettings ->
              aux polarity ms (if polarity then drop 1 args else args) conn
        | m `elem` map fst (view modesPrefixModes modeSettings) ->
              case args of
                [] -> conn -- ?
                arg:args' -> aux polarity ms args'
                           $ recordMessage (modeMsg polarity m arg) target
                           $ over (connChannelIx target . chanUserIx arg)
                                  toggle
                                  conn
        | otherwise -> conn
          where
          toggle
            | polarity = nub . cons m
            | otherwise = delete m

  modeMsg polarity m arg = IrcMessage
         { _mesgType = ModeMsgType polarity m arg
         , _mesgSender = who
         , _mesgStamp = now
         , _mesgModes = ""
         , _mesgMe = False
         }

doBanList :: [IrcBan] -> IrcConnection -> Logic IrcConnection
doBanList acc conn =
  do msg <- getMessage
     case msg of
       RplBanList _ bannee banner stamp ->
         doBanList
           (IrcBan { _banBannee = bannee
                   , _banBanner = banner
                   , _banStamp  = stamp
                   } : acc)
           conn

       RplEndOfBanList chan ->
         return (set (connChannelIx chan . chanBans) (Just (reverse acc)) conn)

       _ -> fail ("Expected ban list end: " ++ show msg)

doNick :: UTCTime -> UserInfo -> ByteString -> IrcConnection -> Logic IrcConnection
doNick stamp who newnick = return
                   . over connUsers updateUsers
                   . over (connChannels . mapped) updateChannel
  where
  oldnick = userNick who

  updateUsers :: Map (CI ByteString) IrcUser -> Map (CI ByteString) IrcUser
  updateUsers users
     = set (at (CI.mk oldnick)) Nothing
     $ set (at (CI.mk newnick))
           (view (at (CI.mk oldnick)) users) users

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

doPart :: UTCTime -> UserInfo -> ByteString -> ByteString -> IrcConnection -> Logic IrcConnection
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

doKick ::
  UTCTime -> UserInfo ->
  ByteString -> ByteString -> ByteString ->
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


doWhoReply :: ByteString -> ByteString -> IrcConnection -> Logic IrcConnection
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

doQuit :: UTCTime -> UserInfo -> ByteString -> IrcConnection -> Logic IrcConnection
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

doAddChannel :: ByteString -> IrcConnection -> Logic IrcConnection
doAddChannel chan conn
  = return
  $ set (connChannelAt chan) (Just channel)
  $ conn
  where
  channel =
    set (chanUserAt (view connNick conn)) (Just "") defaultChannel

doJoinChannel :: UTCTime -> UserInfo -> ByteString -> IrcConnection -> Logic IrcConnection
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
  ByteString ->
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

doMotd :: [ByteString] -> IrcConnection -> Logic IrcConnection
doMotd xs conn =
  do msg <- getMessage
     case msg of
       RplMotd x -> doMotd (x:xs) conn
       RplEndOfMotd -> return (set connMotd (Just (reverse xs)) conn)
       _ -> fail "Expected Motd"

doNameReply :: ByteString -> [ByteString] -> IrcConnection -> Logic IrcConnection
doNameReply chan xs conn =
  do msg <- getMessage
     case msg of
       RplNameReply _ _ x -> doNameReply chan (x++xs) conn
       RplEndOfNames -> return
                     $ set (connChannelIx chan . chanUsers)
                           users
                           conn
       _ -> fail "Expected end of names"
       where
       users = Map.fromList
             $ over (mapped._1) CI.mk
             $ map (splitNamesReplyName conn) xs

splitNamesReplyName :: IrcConnection -> ByteString -> (ByteString, String)
splitNamesReplyName conn = aux []
  where
  aux modes n =
    case B.uncons n of
      Just (x,xs) ->
        case find (\(_mode,symbol) -> x == fromIntegral (ord symbol))
                  (view (connChanModes . modesPrefixModes) conn) of
          Just (mode,_) -> aux (mode:modes) xs
          Nothing       -> (n,modes)
      _                 -> (n,modes)

------------------------------------------------------------------------
-- Type describing computations that will require zero or more messages
-- to perform complex updates to the model.
------------------------------------------------------------------------

runLogic :: Monad m => (LogicOp (m a) -> m a) -> Logic (m a) -> m a
runLogic f (Logic a) = iter f a

data LogicOp r
  = Expect  (MsgFromServer -> r)
  | Failure String
  deriving (Functor)

newtype Logic a = Logic { unLogic :: Free LogicOp a }
  deriving (Functor, Applicative)

instance Monad Logic where
  fail          = Logic . wrap . Failure
  m >>= f       = Logic (unLogic . f =<< unLogic m)
  return        = Logic . return

getMessage :: Logic MsgFromServer
getMessage = Logic (wrap (Expect return))

asUtf8 :: ByteString -> Text
asUtf8 = Text.decodeUtf8With Text.lenientDecode

userInfoBytestring :: UserInfo -> ByteString
userInfoBytestring u = userNick u <> maybe B.empty ("!" <>) (userName u)
                                  <> maybe B.empty ("@" <>) (userHost u)

activeChannelNames :: IrcConnection -> [ByteString]
activeChannelNames = map CI.original . Map.keys . view connChannels

connChannelAt :: Functor f => ByteString -> LensLike' f IrcConnection (Maybe IrcChannel)
connChannelAt k = connChannels . at (CI.mk k)

connChannelIx :: Applicative f => ByteString -> LensLike' f IrcConnection IrcChannel
connChannelIx k = connChannels . ix (CI.mk k)

connUserAt :: Applicative f => ByteString -> LensLike' f IrcConnection (Maybe IrcUser)
connUserAt k = connUsers . at (CI.mk k)

connUserIx :: Applicative f => ByteString -> LensLike' f IrcConnection IrcUser
connUserIx k = connUsers . ix (CI.mk k)

chanUserAt :: Applicative f => ByteString -> LensLike' f IrcChannel (Maybe String)
chanUserAt k = chanUsers . at (CI.mk k)

chanUserIx :: Applicative f => ByteString -> LensLike' f IrcChannel String
chanUserIx k = chanUsers . ix (CI.mk k)




-- | Add a message to the client state for users and channels.
-- The user or channel record should be added if it does not already
-- exist.
recordMessage ::
  IrcMessage ->
  ByteString {- ^ target -} ->
  IrcConnection ->
  IrcConnection
recordMessage mesg target conn =
  case B.uncons target of
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

isMyNick :: ByteString -> IrcConnection -> Bool
isMyNick nick conn = CI.foldCase nick == CI.foldCase (view connNick conn)
