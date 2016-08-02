{-# Language TemplateHaskell, OverloadedStrings, BangPatterns #-}

{-|
Module      : Client.ConnectionState
Description : IRC connection session state
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module is responsible for tracking the state of an individual IRC
connection while the client is connected to it. This state includes
user information, server settings, channel membership, and more.

This module is more complicated than many of the other modules in the
client because it is responsible for interpreting each IRC message from
the server and updating the connection state accordingly.
-}

module Client.ConnectionState
  (
  -- * Connection state type
    ConnectionState(..)
  , csNick
  , csChannels
  , csSocket
  , csModeTypes
  , csChannelTypes
  , csTransaction
  , csModes
  , csStatusMsg
  , csSettings
  , csUserInfo
  , csUsers
  , csUser
  , csModeCount
  , csNetworkId
  , csNetwork
  , csNextPingTime
  , csPingStatus

  , newConnectionState

  -- * User information
  , UserAndHost(..)

  -- * Cross-message state
  , Transaction(..)

  -- * Connection predicates
  , isChannelIdentifier
  , iHaveOp

  -- * Messages interactions
  , sendMsg
  , initialMessages
  , applyMessage
  , squelchIrcMsg

  -- * Timer information
  , PingStatus(..)
  , TimedAction(..)
  , nextTimedAction
  ) where

import           Client.ChannelState
import           Client.NetworkConnection
import           Client.ServerSettings
import           Control.Lens
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.Bits
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import           Data.Time
import           Data.Time.Clock.POSIX
import           Irc.Codes
import           Irc.Commands
import           Irc.Identifier
import           Irc.Message
import           Irc.Modes
import           Irc.RawIrcMsg
import           Irc.UserInfo
import           LensUtils

data ConnectionState = ConnectionState
  { _csNetworkId    :: !NetworkId
  , _csChannels     :: !(HashMap Identifier ChannelState)
  , _csSocket       :: !NetworkConnection
  , _csModeTypes    :: !ModeTypes
  , _csChannelTypes :: ![Char]
  , _csTransaction  :: !Transaction
  , _csModes        :: ![Char]
  , _csStatusMsg    :: ![Char]
  , _csSettings     :: !ServerSettings
  , _csUserInfo     :: !UserInfo
  , _csUsers        :: !(HashMap Identifier UserAndHost)
  , _csModeCount    :: !Int
  , _csNetwork      :: !Text
  , _csNextPingTime :: !(Maybe UTCTime)
  , _csPingStatus   :: !PingStatus
  }
  deriving Show

data UserAndHost = UserAndHost {-# UNPACK #-} !Text {-# UNPACK #-} !Text
  deriving Show

data PingStatus
  = PingSent    !UTCTime
  | PingLatency !Double -- seconds
  | PingNever
  deriving Show

data Transaction
  = NoTransaction
  | NamesTransaction [Text]
  | BanTransaction [(Text,(Text,UTCTime))]
  | WhoTransaction [UserInfo]
  deriving Show

makeLenses ''ConnectionState
makePrisms ''Transaction

csNick :: Lens' ConnectionState Identifier
csNick = csUserInfo . uiNick

sendMsg :: ConnectionState -> RawIrcMsg -> IO ()
sendMsg cs msg =
  case (view msgCommand msg, view msgParams msg) of
    ("PRIVMSG", [tgt,txt]) -> multiline "PRIVMSG" tgt txt
    ("NOTICE",  [tgt,txt]) -> multiline "NOTICE"  tgt txt
    _ -> transmit msg
  where
    transmit = send (view csSocket cs) . renderRawIrcMsg

    multiline cmd tgt txt =
      for_ txtChunks $ \txtChunk ->
        transmit $ rawIrcMsg cmd [tgt, txtChunk]
      where
        txtChunks = utf8ChunksOf maxContentLen txt
        maxContentLen = computeMaxMessageLength (view csUserInfo cs) tgt

-- This is an approximation for splitting the text. It doesn't
-- understand combining characters. A correct implementation
-- probably needs to use icu, but its going to take some work
-- to use that library to do this.
utf8ChunksOf :: Int -> Text -> [Text]
utf8ChunksOf n txt
  | B.length enc <= n = [txt] -- fast/common case
  | otherwise         = search 0 0 txt info
  where
    isBeginning b = b .&. 0xc0 /= 0x80

    enc = Text.encodeUtf8 txt

    beginnings = B.findIndices isBeginning enc

    info = zip3 [0..] -- charIndex
                beginnings
                (drop 1 beginnings ++ [B.length enc])

    search startByte startChar currentTxt xs =
      case dropWhile (\(_,_,byteLen) -> byteLen-startByte <= n) xs of
        [] -> [currentTxt]
        (charIx,byteIx,_):xs' ->
          case Text.splitAt (charIx - startChar) currentTxt of
            (a,b) -> a : search byteIx charIx b xs'

newConnectionState ::
  NetworkId ->
  Text ->
  ServerSettings ->
  NetworkConnection ->
  ConnectionState
newConnectionState networkId network settings sock = ConnectionState
  { _csNetworkId    = networkId
  , _csUserInfo     = UserInfo (mkId (view ssNick settings)) Nothing Nothing
  , _csChannels     = HashMap.empty
  , _csSocket       = sock
  , _csChannelTypes = "#&"
  , _csModeTypes    = defaultModeTypes
  , _csTransaction  = NoTransaction
  , _csModes        = ""
  , _csStatusMsg    = ""
  , _csSettings     = settings
  , _csModeCount    = 3
  , _csUsers        = HashMap.empty
  , _csNetwork      = network
  , _csPingStatus   = PingNever
  , _csNextPingTime = Nothing
  }



noReply :: ConnectionState -> ([RawIrcMsg], ConnectionState)
noReply x = ([], x)

overChannel :: Identifier -> (ChannelState -> ChannelState) -> ConnectionState -> ConnectionState
overChannel chan = overStrict (csChannels . ix chan)

overChannels :: (ChannelState -> ChannelState) -> ConnectionState -> ConnectionState
overChannels = overStrict (csChannels . traverse)

applyMessage :: ZonedTime -> IrcMsg -> ConnectionState -> ([RawIrcMsg], ConnectionState)
applyMessage msgWhen msg cs =
  case msg of
    Ping args -> ([ircPong args], cs)
    Pong _    -> noReply $ doPong msgWhen cs
    Join user chan ->
           noReply
         $ recordUser user
         $ overChannel chan (joinChannel (userNick user))
         $ createOnJoin user chan cs

    Quit user _reason ->
           noReply
         $ forgetUser (userNick user)
         $ overChannels (partChannel (userNick user)) cs

    Part user chan _mbreason ->
           noReply
         $ forgetUser' (userNick user) -- possibly forget
         $ if userNick user == view csNick cs
             then set (csChannels . at chan) Nothing cs
             else overChannel chan (partChannel (userNick user)) cs

    Nick oldNick newNick ->
           noReply
         $ renameUser (userNick oldNick) newNick
         $ updateMyNick (userNick oldNick) newNick
         $ overChannels (nickChange (userNick oldNick) newNick) cs

    Kick _kicker chan nick _reason ->
           noReply
         $ forgetUser' nick
         $ overChannel chan (partChannel nick) cs
    Reply RPL_WELCOME (me:_) -> doWelcome msgWhen (mkId me) cs
    Reply RPL_SASLSUCCESS _ -> ([ircCapEnd], cs)
    Reply RPL_SASLFAIL _ -> ([ircCapEnd], cs)
    Reply code args        -> noReply (doRpl code msgWhen args cs)
    Cap cmd params         -> doCap cmd params cs
    Authenticate param     -> doAuthenticate param cs
    Mode who target (modes:params)  -> doMode msgWhen who target modes params cs
    Topic user chan topic  -> noReply (doTopic msgWhen user chan topic cs)
    _                      -> noReply cs

-- | 001 'RPL_WELCOME' is the first message received when transitioning
-- from the initial handshake to a connected state. At this point we know
-- what nickname the server is using for our connection, and we can start
-- scheduling PINGs.
doWelcome ::
  ZonedTime  {- ^ message received -} ->
  Identifier {- ^ my nickname      -} ->
  ConnectionState -> ([RawIrcMsg], ConnectionState)
doWelcome msgWhen me cs = (reply, update cs)
  where
    reply = mapMaybe parseRawIrcMsg (view (csSettings . ssConnectCmds) cs)

    update
      = set csNick me
      . set csNextPingTime (Just $! addUTCTime 30 (zonedTimeToUTC msgWhen))

doTopic :: ZonedTime -> UserInfo -> Identifier -> Text -> ConnectionState -> ConnectionState
doTopic when user chan topic =
  overChannel chan (setTopic topic . set chanTopicProvenance (Just $! prov))
  where
    prov = TopicProvenance
             { _topicAuthor = user
             , _topicTime   = zonedTimeToUTC when
             }

parseTimeParam :: Text -> Maybe UTCTime
parseTimeParam txt =
  case Text.decimal txt of
    Right (i, rest) | Text.null rest ->
      Just $! posixSecondsToUTCTime (fromInteger i)
    _ -> Nothing

doRpl :: ReplyCode -> ZonedTime -> [Text] -> ConnectionState -> ConnectionState
doRpl cmd msgWhen args =
  case cmd of
    RPL_UMODEIS ->
      case args of
        _me:modes:params
          | Just xs <- splitModes defaultUmodeTypes modes params ->
                 doMyModes xs
               . set csModes "" -- reset modes
        _ -> id

    RPL_NOTOPIC ->
      case args of
        _me:chan:_ -> overChannel
                        (mkId chan)
                        (setTopic "" . set chanTopicProvenance Nothing)
        _          -> id

    RPL_TOPIC ->
      case args of
        _me:chan:topic:_ -> overChannel (mkId chan) (setTopic topic)
        _                -> id

    RPL_TOPICWHOTIME ->
      case args of
        _me:chan:who:whenTxt:_ | Just when <- parseTimeParam whenTxt ->
          let !prov = TopicProvenance
                       { _topicAuthor = parseUserInfo who
                       , _topicTime   = when
                       }
          in overChannel (mkId chan) (set chanTopicProvenance (Just prov))
        _ -> id

    RPL_CREATIONTIME ->
      case args of
        _me:chan:whenTxt:_ | Just when <- parseTimeParam whenTxt ->
          overChannel (mkId chan) (set chanCreation (Just when))
        _ -> id

    RPL_CHANNEL_URL ->
      case args of
        _me:chan:urlTxt:_ ->
          overChannel (mkId chan) (set chanUrl (Just urlTxt))
        _ -> id

    RPL_ISUPPORT -> isupport args

    RPL_NAMREPLY ->
      case args of
        _me:_sym:_tgt:x:_ ->
           over csTransaction
                (\t -> let xs = view _NamesTransaction t
                       in xs `seq` NamesTransaction (x:xs))
        _ -> id

    RPL_ENDOFNAMES ->
      case args of
        _me:tgt:_ -> loadNamesList (mkId tgt)
        _         -> id

    RPL_BANLIST ->
      case args of
        _me:_tgt:mask:who:whenTxt:_ -> recordListEntry mask who whenTxt
        _                           -> id

    RPL_ENDOFBANLIST ->
      case args of
        _me:tgt:_ -> saveList 'b' tgt
        _         -> id

    RPL_QUIETLIST ->
      case args of
        _me:_tgt:_q:mask:who:whenTxt:_ -> recordListEntry mask who whenTxt
        _                              -> id

    RPL_ENDOFQUIETLIST ->
      case args of
        _me:tgt:_ -> saveList 'q' tgt
        _         -> id

    RPL_INVEXLIST ->
      case args of
        _me:_tgt:mask:who:whenTxt:_ -> recordListEntry mask who whenTxt
        _                           -> id

    RPL_ENDOFINVEXLIST ->
      case args of
        _me:tgt:_ -> saveList 'I' tgt
        _         -> id

    RPL_EXCEPTLIST ->
      case args of
        _me:_tgt:mask:who:whenTxt:_ -> recordListEntry mask who whenTxt
        _                           -> id

    RPL_ENDOFEXCEPTLIST ->
      case args of
        _me:tgt:_ -> saveList 'e' tgt
        _         -> id

    RPL_WHOREPLY ->
      case args of
        _me:_tgt:uname:host:_server:nick:_ ->
          over csTransaction $ \t ->
            let !xs = view _WhoTransaction t
            in WhoTransaction (UserInfo (mkId nick) (Just uname) (Just host) : xs)
        _ -> id

    RPL_ENDOFWHO -> massRegistration

    RPL_CHANNELMODEIS ->
      case args of
        _me:chan:modes:params ->
              snd -- channel mode reply shouldn't trigger messages
            . doMode msgWhen (UserInfo (mkId "*") Nothing Nothing) chanId modes params
            . set (csChannels . ix chanId . chanModes) Map.empty
            where chanId = mkId chan
        _ -> id
    _ -> id


-- | Add an entry to a mode list transaction
recordListEntry ::
  Text {- ^ mask -} ->
  Text {- ^ set by -} ->
  Text {- ^ set time -} ->
  ConnectionState -> ConnectionState
recordListEntry mask who whenTxt =
  case parseTimeParam whenTxt of
    Nothing   -> id
    Just when ->
      over csTransaction $ \t ->
        let !xs = view _BanTransaction t
        in BanTransaction ((mask,(who,when)):xs)


-- | Save a completed ban, quiet, invex, or exempt list into the channel
-- state.
saveList ::
  Char {- ^ mode -} ->
  Text {- ^ channel -} ->
  ConnectionState -> ConnectionState
saveList mode tgt cs
   = set csTransaction NoTransaction
   $ setStrict
        (csChannels . ix (mkId tgt) . chanLists . at mode)
        (Just $! newList)
        cs
  where
    newList = HashMap.fromList (view (csTransaction . _BanTransaction) cs)


-- | These replies are interpreted by the client and should only be shown
-- in the detailed view.
squelchReply :: ReplyCode -> Bool
squelchReply rpl =
  case rpl of
    RPL_NAMREPLY        -> True
    RPL_ENDOFNAMES      -> True
    RPL_BANLIST         -> True
    RPL_ENDOFBANLIST    -> True
    RPL_INVEXLIST       -> True
    RPL_ENDOFINVEXLIST  -> True
    RPL_EXCEPTLIST      -> True
    RPL_ENDOFEXCEPTLIST -> True
    RPL_QUIETLIST       -> True
    RPL_ENDOFQUIETLIST  -> True
    RPL_CHANNELMODEIS   -> True
    RPL_TOPIC           -> True
    RPL_TOPICWHOTIME    -> True
    RPL_CREATIONTIME    -> True
    RPL_CHANNEL_URL     -> True
    RPL_NOTOPIC         -> True
    RPL_UMODEIS         -> True
    RPL_WHOREPLY        -> True
    RPL_ENDOFWHO        -> True
    _                   -> False

-- | Return 'True' for messages that should be hidden outside of
-- full detail view. These messages are interpreted by the client
-- so the user shouldn't need to see them directly to get the
-- relevant information.
squelchIrcMsg :: IrcMsg -> Bool
squelchIrcMsg (Reply rpl _) = squelchReply rpl
squelchIrcMsg _             = False

doMode ::
  ZonedTime {- ^ time of message -} ->
  UserInfo  {- ^ sender          -} ->
  Identifier {- ^ channel        -} ->
  Text       {- ^ mode flags     -} ->
  [Text]     {- ^ mode parameters -} ->
  ConnectionState -> ([RawIrcMsg], ConnectionState)
doMode when who target modes args cs
  | view csNick cs == target
  , Just xs <- splitModes defaultUmodeTypes modes args =
        noReply (doMyModes xs cs)

  | isChannelIdentifier cs target
  , Just xs <- splitModes (view csModeTypes cs) modes args =
        let cs' = doChannelModes when who target xs cs

            finish | iHaveOp target cs' = popQueue
                   | otherwise          = noReply

        in finish cs'
  where
    popQueue :: ConnectionState -> ([RawIrcMsg], ConnectionState)
    popQueue = csChannels . ix target . chanQueuedModeration <<.~ []

doMode _ _ _ _ _ cs = noReply cs -- ignore bad mode command

-- | Predicate to test if the connection has op in a given channel.
iHaveOp :: Identifier -> ConnectionState -> Bool
iHaveOp channel cs =
  elemOf (csChannels . ix channel . chanUsers . ix me . folded) '@' cs
  where
    me = view csNick cs


doChannelModes :: ZonedTime -> UserInfo -> Identifier -> [(Bool, Char, Text)] -> ConnectionState -> ConnectionState
doChannelModes when who chan changes cs = overChannel chan applyChannelModes cs
  where
    modeTypes = view csModeTypes cs
    sigilMap  = view modesPrefixModes modeTypes
    listModes = view modesLists modeTypes

    applyChannelModes c = foldl' applyChannelMode c changes

    applyChannelMode c (polarity, mode, arg)

      | Just sigil <- lookup mode sigilMap =
          overStrict (chanUsers . ix (mkId arg))
                     (setPrefixMode polarity sigil)
                     c

      | mode `elem` listModes =
        let entry | polarity = Just (renderUserInfo who, zonedTimeToUTC when)
                  | otherwise = Nothing
        in setStrict (chanLists . ix mode . at arg) entry c

      | polarity  = set (chanModes . at mode) (Just arg) c
      | otherwise = set (chanModes . at mode) Nothing    c

    setPrefixMode polarity sigil sigils
      | not polarity        = delete sigil sigils
      | sigil `elem` sigils = sigils
      | otherwise           = filter (`elem` sigils') (map snd sigilMap)
      where
        sigils' = sigil : sigils


doMyModes :: [(Bool, Char, Text)] -> ConnectionState -> ConnectionState
doMyModes changes = over csModes $ \modes -> sort (foldl' applyOne modes changes)
  where
    applyOne modes (True, mode, _)
      | mode `elem` modes = modes
      | otherwise         = mode:modes
    applyOne modes (False, mode, _) = delete mode modes

supportedCaps :: ConnectionState -> [Text]
supportedCaps cs = sasl ++ ["multi-prefix", "znc.in/playback", "znc.in/server-time-iso"]
  where
    ss = view csSettings cs
    sasl = ["sasl" | isJust (view ssSaslUsername ss)
                   , isJust (view ssSaslPassword ss) ]

doAuthenticate :: Text -> ConnectionState -> ([RawIrcMsg], ConnectionState)
doAuthenticate "+" cs
  | Just user <- view ssSaslUsername ss
  , Just pass <- view ssSaslPassword ss
  = ([ircAuthenticate (encodePlainAuthentication user pass)], cs)
  where
    ss = view csSettings cs

doAuthenticate _ cs = ([ircCapEnd], cs)

doCap :: CapCmd -> [Text] -> ConnectionState -> ([RawIrcMsg], ConnectionState)
doCap cmd args cs =
  case (cmd,args) of
    (CapLs,[capsTxt])
      | null reqCaps -> ([ircCapEnd], cs)
      | otherwise -> ([ircCapReq reqCaps], cs)
      where
        caps = Text.words capsTxt
        reqCaps = intersect (supportedCaps cs) caps

    (CapAck,[capsTxt])
      | "sasl" `elem` caps ->
          ([ircAuthenticate plainAuthenticationMode], cs)
      where
        caps = Text.words capsTxt

    _ -> ([ircCapEnd], cs)


initialMessages :: ConnectionState -> [RawIrcMsg]
initialMessages cs
   = [ ircCapLs ]
  ++ [ ircPass pass | Just pass <- [view ssPassword ss]]
  ++ [ ircUser (view ssUser ss) False True (view ssReal ss)
     , ircNick (view csNick cs)
     ]
  where
    ss = view csSettings cs

loadNamesList :: Identifier -> ConnectionState -> ConnectionState
loadNamesList chan cs
  = set csTransaction NoTransaction
  $ setStrict (csChannels . ix chan . chanUsers) newChanUsers
  $ cs
  where
    newChanUsers = HashMap.fromList (splitEntry "" <$> entries)

    sigils = toListOf (csModeTypes . modesPrefixModes . folded . _2) cs

    splitEntry modes str
      | Text.head str `elem` sigils = splitEntry (Text.head str : modes)
                                                 (Text.tail str)
      | otherwise = (mkId str, reverse modes)

    entries =
      concatMap Text.words (view (csTransaction . _NamesTransaction) cs)


createOnJoin :: UserInfo -> Identifier -> ConnectionState -> ConnectionState
createOnJoin who chan cs
  | userNick who == view csNick cs =
        set csUserInfo who -- great time to learn our userinfo
      $ set (csChannels . at chan) (Just newChannel) cs
  | otherwise = cs

updateMyNick :: Identifier -> Identifier -> ConnectionState -> ConnectionState
updateMyNick oldNick newNick cs
  | oldNick == view csNick cs = set csNick newNick cs
  | otherwise = cs

-- ISUPPORT is defined by
-- https://tools.ietf.org/html/draft-brocklesby-irc-isupport-03#section-3.14
isupport ::
  [Text] {- ^ ["key=value"] -} ->
  ConnectionState ->
  ConnectionState
isupport []     conn = conn
isupport params conn = foldl' (flip isupport1) conn
                     $ map parseISupport
                     $ init params
  where
    isupport1 ("CHANTYPES",types) = set csChannelTypes (Text.unpack types)
    isupport1 ("CHANMODES",modes) = updateChanModes modes
    isupport1 ("PREFIX"   ,modes) = updateChanPrefix modes
    isupport1 ("STATUSMSG",prefix) = set csStatusMsg (Text.unpack prefix)
    isupport1 ("MODES",nstr) | Right (n,"") <- Text.decimal nstr =
                        set csModeCount n
    isupport1 _                   = id

parseISupport :: Text -> (Text,Text)
parseISupport str =
  case Text.break (=='=') str of
    (key,val) -> (key, Text.drop 1 val)

updateChanModes ::
  Text {- lists,always,set,never -} ->
  ConnectionState ->
  ConnectionState
updateChanModes modes
  = over csModeTypes
  $ set modesLists listModes
  . set modesAlwaysArg alwaysModes
  . set modesSetArg setModes
  . set modesNeverArg neverModes
  -- Note: doesn't set modesPrefixModes
  where
  next = over _2 (drop 1) . break (==',')
  (listModes  ,modes1) = next (Text.unpack modes)
  (alwaysModes,modes2) = next modes1
  (setModes   ,modes3) = next modes2
  (neverModes ,_)      = next modes3

updateChanPrefix ::
  Text {- e.g. "(ov)@+" -} ->
  ConnectionState ->
  ConnectionState
updateChanPrefix txt =
  case parsePrefixes txt of
    Just prefixes -> set (csModeTypes . modesPrefixModes) prefixes
    Nothing       -> id

parsePrefixes :: Text -> Maybe [(Char,Char)]
parsePrefixes txt =
  case uncurry Text.zip (Text.break (==')') txt) of
    ('(',')'):rest -> Just rest
    _              -> Nothing

isChannelIdentifier :: ConnectionState -> Identifier -> Bool
isChannelIdentifier cs ident =
  case Text.uncons (idText ident) of
    Just (p, _) -> p `elem` view csChannelTypes cs
    _           -> False

------------------------------------------------------------------------
-- Helpers for managing the user list
------------------------------------------------------------------------

csUser :: Functor f => Identifier -> LensLike' f ConnectionState (Maybe UserAndHost)
csUser i = csUsers . at i

recordUser :: UserInfo -> ConnectionState -> ConnectionState
recordUser user =
  case (userName user, userHost user) of
    (Just u, Just h) -> set (csUsers . at (userNick user))
                            (Just (UserAndHost u h))
    _ -> id

forgetUser :: Identifier -> ConnectionState -> ConnectionState
forgetUser nick = set (csUsers . at nick) Nothing

renameUser :: Identifier -> Identifier -> ConnectionState -> ConnectionState
renameUser old new cs = set (csUsers . at new) entry cs'
  where
    (entry,cs') = cs & csUsers . at old <<.~ Nothing

forgetUser' :: Identifier -> ConnectionState -> ConnectionState
forgetUser' nick cs
  | keep      = cs
  | otherwise = forgetUser nick cs
  where
    keep = has (csChannels . folded . chanUsers . ix nick) cs

-- | Process a list of WHO replies
massRegistration :: ConnectionState -> ConnectionState
massRegistration cs
  = set csTransaction NoTransaction
  $ over csUsers updateUsers cs
  where
    infos = view (csTransaction . _WhoTransaction) cs

    channelUsers =
      HashSet.fromList (views (csChannels . folded . chanUsers) HashMap.keys cs)

    updateUsers users = foldl' updateUser users infos

    updateUser users !info
      | Just u <- userName info
      , Just h <- userHost info
      , HashSet.member (userNick info) channelUsers =
              HashMap.insert (userNick info) (UserAndHost u h) users
      | otherwise = users

-- | Timer-based events
data TimedAction
  = TimedDisconnect -- ^ terminate the connection due to timeout
  | TimedSendPing -- ^ transmit a ping to the server
  deriving (Eq, Ord, Show)

-- | Compute the earliest timed action for a connection, if any
nextTimedAction :: ConnectionState -> Maybe (UTCTime, TimedAction)
nextTimedAction cs =
  do runAt <- view csNextPingTime cs
     return (runAt, action)
  where
    action =
      case view csPingStatus cs of
        PingSent{}    -> TimedDisconnect
        PingLatency{} -> TimedSendPing
        PingNever     -> TimedSendPing

doPong :: ZonedTime -> ConnectionState -> ConnectionState
doPong when cs = set csPingStatus (PingLatency delta) cs
  where
    delta =
      case view csPingStatus cs of
        PingSent sent -> realToFrac (diffUTCTime (zonedTimeToUTC when) sent)
        _ -> 0
