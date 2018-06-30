{-# Language TemplateHaskell, OverloadedStrings, BangPatterns #-}

{-|
Module      : Client.State.Network
Description : IRC network session state
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

module Client.State.Network
  (
  -- * Connection state
    NetworkState(..)
  , AuthenticateState(..)
  , newNetworkState

  -- * Lenses
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
  , csLatency
  , csLastReceived
  , csMessageHooks
  , csAuthenticationState

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
  , _PingConnecting
  , TimedAction(..)
  , nextTimedAction
  , applyTimedAction

  -- * Moderation
  , useChanServ
  , sendModeration
  , sendTopic
  ) where

import qualified Client.Authentication.Ecdsa as Ecdsa
import           Client.Configuration.ServerSettings
import           Client.Network.Async
import           Client.State.Channel
import           Control.Lens
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import           Data.Bits
import           Data.Foldable
import           Data.List
import qualified Data.List.NonEmpty as NonEmpty
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

-- | State tracked for each IRC connection
data NetworkState = NetworkState
  { _csNetworkId    :: !NetworkId -- ^ network connection identifier
  , _csChannels     :: !(HashMap Identifier ChannelState) -- ^ joined channels
  , _csSocket       :: !NetworkConnection -- ^ network socket
  , _csModeTypes    :: !ModeTypes -- ^ channel mode meanings
  , _csChannelTypes :: ![Char] -- ^ channel identifier prefixes
  , _csTransaction  :: !Transaction -- ^ state for multi-message sequences
  , _csModes        :: ![Char] -- ^ modes for the connected user
  , _csStatusMsg    :: ![Char] -- ^ modes that prefix statusmsg channel names
  , _csSettings     :: !ServerSettings -- ^ settings used for this connection
  , _csUserInfo     :: !UserInfo -- ^ usermask used by the server for this connection
  , _csUsers        :: !(HashMap Identifier UserAndHost) -- ^ user and hostname for other nicks
  , _csModeCount    :: !Int -- ^ maximum mode changes per MODE command
  , _csNetwork      :: !Text -- ^ name of network connection
  , _csMessageHooks :: ![Text] -- ^ names of message hooks to apply to this connection
  , _csAuthenticationState :: !AuthenticateState

  -- Timing information
  , _csNextPingTime :: !(Maybe UTCTime) -- ^ time for next ping event
  , _csLatency      :: !(Maybe NominalDiffTime) -- ^ latency calculated from previous pong
  , _csPingStatus   :: !PingStatus      -- ^ state of ping timer
  , _csLastReceived :: !(Maybe UTCTime) -- ^ time of last message received
  }
  deriving Show

-- | State of the authentication transaction
data AuthenticateState
  = AS_None               -- ^ no active transaction
  | AS_PlainStarted       -- ^ PLAIN mode initiated
  | AS_EcdsaStarted       -- ^ ECDSA-NIST mode initiated
  | AS_EcdsaWaitChallenge -- ^ ECDSA-NIST user sent waiting for challenge
  deriving Show

-- | Pair of username and hostname. Empty strings represent missing information.
data UserAndHost =
  UserAndHost {-# UNPACK #-} !Text {-# UNPACK #-} !Text
  -- ^ username hostname
  deriving Show

-- | Status of the ping timer
data PingStatus
  = PingSent !UTCTime -- ^ ping sent at given time, waiting for pong
  | PingNone          -- ^ not waiting for a pong
  | PingConnecting !Int !(Maybe UTCTime) -- ^ number of attempts, last known connection time
  deriving Show


data Transaction
  = NoTransaction
  | NamesTransaction [Text]
  | BanTransaction [(Text,MaskListEntry)]
  | WhoTransaction [UserInfo]
  deriving Show

makeLenses ''NetworkState
makePrisms ''Transaction
makePrisms ''PingStatus

defaultChannelTypes :: String
defaultChannelTypes = "#&"

csNick :: Lens' NetworkState Identifier
csNick = csUserInfo . uiNick

-- | Transmit a 'RawIrcMsg' on the connection associated
-- with the given network. For @PRIVMSG@ and @NOTICE@ overlong
-- commands are detected and transmitted as multiple messages.
sendMsg :: NetworkState -> RawIrcMsg -> IO ()
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

-- | Construct a new network state using the given settings and
-- default values as specified by the IRC specification.
newNetworkState ::
  NetworkId         {- ^ unique network ID         -} ->
  Text              {- ^ network name              -} ->
  ServerSettings    {- ^ server settings           -} ->
  NetworkConnection {- ^ active network connection -} ->
  PingStatus        {- ^ initial ping status       -} ->
  NetworkState      {- ^ new network state         -}
newNetworkState networkId network settings sock ping = NetworkState
  { _csNetworkId    = networkId
  , _csUserInfo     = UserInfo "*" "" ""
  , _csChannels     = HashMap.empty
  , _csSocket       = sock
  , _csChannelTypes = defaultChannelTypes
  , _csModeTypes    = defaultModeTypes
  , _csTransaction  = NoTransaction
  , _csModes        = ""
  , _csStatusMsg    = ""
  , _csSettings     = settings
  , _csModeCount    = 3
  , _csUsers        = HashMap.empty
  , _csNetwork      = network
  , _csMessageHooks = view ssMessageHooks settings
  , _csAuthenticationState = AS_None
  , _csPingStatus   = ping
  , _csLatency      = Nothing
  , _csNextPingTime = Nothing
  , _csLastReceived = Nothing
  }


-- | Used for updates to a 'NetworkState' that require no reply.
--
-- @noReply x = ([], x)@
noReply :: NetworkState -> ([RawIrcMsg], NetworkState)
noReply x = ([], x)

overChannel :: Identifier -> (ChannelState -> ChannelState) -> NetworkState -> NetworkState
overChannel chan = overStrict (csChannels . ix chan)

overChannels :: (ChannelState -> ChannelState) -> NetworkState -> NetworkState
overChannels = overStrict (csChannels . traverse)

applyMessage :: ZonedTime -> IrcMsg -> NetworkState -> ([RawIrcMsg], NetworkState)
applyMessage msgWhen msg cs
  = applyMessage' msgWhen msg
  $ set csLastReceived (Just $! zonedTimeToUTC msgWhen) cs

applyMessage' :: ZonedTime -> IrcMsg -> NetworkState -> ([RawIrcMsg], NetworkState)
applyMessage' msgWhen msg cs =
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

    Part user chan _mbreason -> exitChannel chan (userNick user)

    Kick _kicker chan nick _reason -> exitChannel chan nick

    Nick oldNick newNick ->
           noReply
         $ renameUser (userNick oldNick) newNick
         $ updateMyNick (userNick oldNick) newNick
         $ overChannels (nickChange (userNick oldNick) newNick) cs

    Reply RPL_WELCOME (me:_) -> doWelcome msgWhen (mkId me) cs
    Reply RPL_SASLSUCCESS _ -> ([ircCapEnd], cs)
    Reply RPL_SASLFAIL _ -> ([ircCapEnd], cs)
    Reply ERR_NICKNAMEINUSE (_:badnick:_)
      | PingConnecting{} <- view csPingStatus cs -> doBadNick badnick cs
    Reply code args        -> noReply (doRpl code msgWhen args cs)
    Cap cmd params         -> doCap cmd params cs
    Authenticate param     -> doAuthenticate param cs
    Mode who target (modes:params)  -> doMode msgWhen who target modes params cs
    Topic user chan topic  -> noReply (doTopic msgWhen user chan topic cs)
    _                      -> noReply cs
  where
    exitChannel chan nick
      | nick == view csNick cs = noReply $ pruneUsers
                               $ over csChannels (sans chan) cs

      | otherwise              = noReply $ forgetUser' nick
                               $ overChannel chan (partChannel nick) cs

-- | Restrict 'csUsers' to only users are in a channel that the client
-- is connected to.
pruneUsers :: NetworkState -> NetworkState
pruneUsers cs = over csUsers (`HashMap.intersection` u) cs
  where
    u = foldOf (csChannels . folded . chanUsers) cs

-- | 001 'RPL_WELCOME' is the first message received when transitioning
-- from the initial handshake to a connected state. At this point we know
-- what nickname the server is using for our connection, and we can start
-- scheduling PINGs.
doWelcome ::
  ZonedTime  {- ^ message received -} ->
  Identifier {- ^ my nickname      -} ->
  NetworkState -> ([RawIrcMsg], NetworkState)
doWelcome msgWhen me
  = noReply
  . set csNick me
  . set csNextPingTime (Just $! addUTCTime 30 (zonedTimeToUTC msgWhen))
  . set csPingStatus PingNone

-- | Handle 'ERR_NICKNAMEINUSE' errors when connecting.
doBadNick ::
  Text {- ^ bad nickname -} ->
  NetworkState ->
  ([RawIrcMsg], NetworkState) {- ^ replies, updated state -}
doBadNick badNick cs =
  case NonEmpty.dropWhile (badNick/=) (view (csSettings . ssNicks) cs) of
    _:next:_ -> ([ircNick next], cs)
    _        -> ([], cs)

doTopic :: ZonedTime -> UserInfo -> Identifier -> Text -> NetworkState -> NetworkState
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

doRpl :: ReplyCode -> ZonedTime -> [Text] -> NetworkState -> NetworkState
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
            let !x  = UserInfo (mkId nick) uname host
                !xs = view _WhoTransaction t
            in WhoTransaction (x : xs)
        _ -> id

    RPL_ENDOFWHO -> massRegistration

    RPL_CHANNELMODEIS ->
      case args of
        _me:chan:modes:params ->
              snd -- channel mode reply shouldn't trigger messages
            . doMode msgWhen who chanId modes params
            . set (csChannels . ix chanId . chanModes) Map.empty
            where chanId = mkId chan
                  !who = UserInfo "*" "" ""
        _ -> id
    _ -> id


-- | Add an entry to a mode list transaction
recordListEntry ::
  Text {- ^ mask -} ->
  Text {- ^ set by -} ->
  Text {- ^ set time -} ->
  NetworkState -> NetworkState
recordListEntry mask who whenTxt =
  case parseTimeParam whenTxt of
    Nothing   -> id
    Just when ->
      over csTransaction $ \t ->
        let !x = MaskListEntry
                    { _maskListSetter = who
                    , _maskListTime   = when
                    }
            !xs = view _BanTransaction t
        in BanTransaction ((mask,x):xs)


-- | Save a completed ban, quiet, invex, or exempt list into the channel
-- state.
saveList ::
  Char {- ^ mode -} ->
  Text {- ^ channel -} ->
  NetworkState -> NetworkState
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
  NetworkState -> ([RawIrcMsg], NetworkState)
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
    popQueue :: NetworkState -> ([RawIrcMsg], NetworkState)
    popQueue = csChannels . ix target . chanQueuedModeration <<.~ []

doMode _ _ _ _ _ cs = noReply cs -- ignore bad mode command

-- | Predicate to test if the connection has op in a given channel.
iHaveOp :: Identifier -> NetworkState -> Bool
iHaveOp channel cs =
  elemOf (csChannels . ix channel . chanUsers . ix me . folded) '@' cs
  where
    me = view csNick cs


doChannelModes :: ZonedTime -> UserInfo -> Identifier -> [(Bool, Char, Text)] -> NetworkState -> NetworkState
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
        let entry | polarity = Just $! MaskListEntry
                         { _maskListSetter = renderUserInfo who
                         , _maskListTime   = zonedTimeToUTC when
                         }
                  | otherwise = Nothing
        in setStrict (chanLists . ix mode . at arg) entry c

      | polarity  = set (chanModes . at mode) (Just arg) c
      | otherwise = over chanModes (sans mode) c

    setPrefixMode polarity sigil sigils
      | not polarity        = delete sigil sigils
      | sigil `elem` sigils = sigils
      | otherwise           = filter (`elem` sigils') (map snd sigilMap)
      where
        sigils' = sigil : sigils


doMyModes :: [(Bool, Char, Text)] -> NetworkState -> NetworkState
doMyModes changes = over csModes $ \modes -> sort (foldl' applyOne modes changes)
  where
    applyOne modes (True, mode, _)
      | mode `elem` modes = modes
      | otherwise         = mode:modes
    applyOne modes (False, mode, _) = delete mode modes

supportedCaps :: NetworkState -> [Text]
supportedCaps cs =
  sasl ++ ["multi-prefix", "znc.in/batch", "znc.in/playback",
           "znc.in/server-time-iso", "znc.in/self-message"]
  where
    ss = view csSettings cs
    sasl = ["sasl" | isJust (view ssSaslUsername ss)
                   , isJust (view ssSaslPassword ss) ||
                     isJust (view ssSaslEcdsaFile ss) ]

doAuthenticate :: Text -> NetworkState -> ([RawIrcMsg], NetworkState)
doAuthenticate param cs =
  case view csAuthenticationState cs of
    AS_PlainStarted
      | "+" <- param
      , Just user <- view ssSaslUsername ss
      , Just pass <- view ssSaslPassword ss
      -> ([ircAuthenticate (encodePlainAuthentication user pass)],
          set csAuthenticationState AS_None cs)

    AS_EcdsaStarted
      | "+" <- param
      , Just user <- view ssSaslUsername ss
      -> ([ircAuthenticate (Ecdsa.encodeUsername user)],
          set csAuthenticationState AS_EcdsaWaitChallenge cs)

    AS_EcdsaWaitChallenge -> ([], cs) -- handled in Client.EventLoop!

    _ -> ([ircCapEnd], cs) -- really shouldn't happen

  where
    ss = view csSettings cs


doCap :: CapCmd -> [Text] -> NetworkState -> ([RawIrcMsg], NetworkState)
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
          if isJust (view ssSaslEcdsaFile ss)
            then ([ircAuthenticate Ecdsa.authenticationMode], set csAuthenticationState AS_EcdsaStarted cs)
          else if isJust (view ssSaslUsername ss)
            then ([ircAuthenticate plainAuthenticationMode], set csAuthenticationState AS_PlainStarted cs)
          else ([ircCapEnd], cs)
      where
        ss   = view csSettings cs
        caps = Text.words capsTxt

    _ -> ([ircCapEnd], cs)


initialMessages :: NetworkState -> [RawIrcMsg]
initialMessages cs
   = [ ircCapLs ]
  ++ [ ircPass pass | Just pass <- [view ssPassword ss]]
  ++ [ ircNick (views ssNicks NonEmpty.head ss)
     , ircUser (view ssUser ss) False True (view ssReal ss)
     ]
  where
    ss = view csSettings cs

loadNamesList :: Identifier -> NetworkState -> NetworkState
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


createOnJoin :: UserInfo -> Identifier -> NetworkState -> NetworkState
createOnJoin who chan cs
  | userNick who == view csNick cs =
        set csUserInfo who -- great time to learn our userinfo
      $ set (csChannels . at chan) (Just newChannel) cs
  | otherwise = cs

updateMyNick :: Identifier -> Identifier -> NetworkState -> NetworkState
updateMyNick oldNick newNick cs
  | oldNick == view csNick cs = set csNick newNick cs
  | otherwise = cs

-- ISUPPORT is defined by
-- https://tools.ietf.org/html/draft-brocklesby-irc-isupport-03#section-3.14
isupport ::
  [Text] {- ^ ["key=value"] -} ->
  NetworkState ->
  NetworkState
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
  NetworkState ->
  NetworkState
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
  NetworkState ->
  NetworkState
updateChanPrefix txt =
  case parsePrefixes txt of
    Just prefixes -> set (csModeTypes . modesPrefixModes) prefixes
    Nothing       -> id

parsePrefixes :: Text -> Maybe [(Char,Char)]
parsePrefixes txt =
  case uncurry Text.zip (Text.break (==')') txt) of
    ('(',')'):rest -> Just rest
    _              -> Nothing

isChannelIdentifier :: NetworkState -> Identifier -> Bool
isChannelIdentifier cs ident =
  case Text.uncons (idText ident) of
    Just (p, _) -> p `elem` view csChannelTypes cs
    _           -> False

------------------------------------------------------------------------
-- Helpers for managing the user list
------------------------------------------------------------------------

csUser :: Functor f => Identifier -> LensLike' f NetworkState (Maybe UserAndHost)
csUser i = csUsers . at i

recordUser :: UserInfo -> NetworkState -> NetworkState
recordUser (UserInfo nick user host)
  | Text.null user || Text.null host = id
  | otherwise = set (csUsers . at nick)
                    (Just (UserAndHost user host))

forgetUser :: Identifier -> NetworkState -> NetworkState
forgetUser = over csUsers . sans

renameUser :: Identifier -> Identifier -> NetworkState -> NetworkState
renameUser old new cs = set (csUsers . at new) entry cs'
  where
    (entry,cs') = cs & csUsers . at old <<.~ Nothing

forgetUser' :: Identifier -> NetworkState -> NetworkState
forgetUser' nick cs
  | keep      = cs
  | otherwise = forgetUser nick cs
  where
    keep = has (csChannels . folded . chanUsers . ix nick) cs

-- | Process a list of WHO replies
massRegistration :: NetworkState -> NetworkState
massRegistration cs
  = set csTransaction NoTransaction
  $ over csUsers updateUsers cs
  where
    infos = view (csTransaction . _WhoTransaction) cs

    channelUsers =
      HashSet.fromList (views (csChannels . folded . chanUsers) HashMap.keys cs)

    updateUsers users = foldl' updateUser users infos

    updateUser users (UserInfo nick user host)
      | not (Text.null user)
      , not (Text.null host)
      , HashSet.member nick channelUsers =
              HashMap.insert nick (UserAndHost user host) users
      | otherwise = users

-- | Timer-based events
data TimedAction
  = TimedDisconnect -- ^ terminate the connection due to timeout
  | TimedSendPing -- ^ transmit a ping to the server
  deriving (Eq, Ord, Show)

-- | Compute the earliest timed action for a connection, if any
nextTimedAction :: NetworkState -> Maybe (UTCTime, TimedAction)
nextTimedAction cs =
  do runAt <- view csNextPingTime cs
     return (runAt, action)
  where
    action =
      case view csPingStatus cs of
        PingSent{}       -> TimedDisconnect
        PingNone         -> TimedSendPing
        PingConnecting{} -> TimedSendPing

doPong :: ZonedTime -> NetworkState -> NetworkState
doPong when cs = set csPingStatus PingNone
               $ set csLatency (Just delta) cs
  where
    delta =
      case view csPingStatus cs of
        PingSent sent -> diffUTCTime (zonedTimeToUTC when) sent
        _             -> 0

-- | Apply the given 'TimedAction' to a connection state.
applyTimedAction :: TimedAction -> NetworkState -> IO NetworkState
applyTimedAction action cs =
  case action of
    TimedDisconnect ->
      do abortConnection PingTimeout (view csSocket cs)
         return $! set csNextPingTime Nothing cs

    TimedSendPing ->
      do now <- getCurrentTime
         sendMsg cs (ircPing ["ping"])
         return $! set csNextPingTime (Just $! addUTCTime 60 now)
                $  set csPingStatus   (PingSent now) cs

------------------------------------------------------------------------
-- Moderation
------------------------------------------------------------------------

-- | Used to send commands that require ops to perform.
-- If this channel is one that the user has chanserv access and ops are needed
-- then ops are requested and the commands are queued, otherwise send them
-- directly.
sendModeration ::
  Identifier      {- ^ channel       -} ->
  [RawIrcMsg]     {- ^ commands      -} ->
  NetworkState    {- ^ network state -} ->
  IO NetworkState
sendModeration channel cmds cs
  | useChanServ channel cs =
      do let cmd = ircPrivmsg "ChanServ" (Text.unwords ["OP", idText channel])
         sendMsg cs cmd
         return $ csChannels . ix channel . chanQueuedModeration <>~ cmds $ cs
  | otherwise = cs <$ traverse_ (sendMsg cs) cmds

useChanServ ::
  Identifier   {- ^ channel            -} ->
  NetworkState {- ^ network state      -} ->
  Bool         {- ^ chanserv available -}
useChanServ channel cs =
  channel `elem` view (csSettings . ssChanservChannels) cs &&
  not (iHaveOp channel cs)

sendTopic ::
  Identifier   {- ^ channel       -} ->
  Text         {- ^ topic         -} ->
  NetworkState {- ^ network state -} ->
  IO ()
sendTopic channelId topic cs = sendMsg cs cmd
  where
    chanservTopicCmd =
      ircPrivmsg
        "ChanServ"
        (Text.unwords ["TOPIC", idText channelId, topic])

    cmd
      | Text.null topic          = ircTopic channelId ""
      | useChanServ channelId cs = chanservTopicCmd
      | otherwise                = ircTopic channelId topic
