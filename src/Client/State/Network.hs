{-# Language BlockArguments, TemplateHaskell, OverloadedStrings, BangPatterns #-}

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
  , ConnectRestriction(..)
  , newNetworkState

  -- * Lenses
  , csNick
  , csChannels
  , csChannelList
  , csWhoReply
  , csSocket
  , csModeTypes
  , csChannelTypes
  , csTransaction
  , csModes
  , csSnomask
  , csStatusMsg
  , csSettings
  , csUserInfo
  , csUsers
  , csUser
  , csModeCount
  , csNetwork
  , csNextPingTime
  , csPingStatus
  , csLatency
  , csLastReceived
  , csCertificate
  , csMessageHooks
  , csAuthenticationState
  , csSeed
  , csAway
  , clsElist
  , clsDone
  , clsItems

  -- * Cross-message state
  , Transaction(..)

  -- * Connection predicates
  , isChannelIdentifier
  , iHaveOp

  -- * Messages interactions
  , sendMsg
  , initialMessages
  , squelchIrcMsg

  -- * NetworkState update
  , Apply(..)
  , applyMessage
  , hideMessage

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

import Client.Authentication.Ecdh qualified as Ecdh
import Client.Authentication.Ecdsa qualified as Ecdsa
import Client.Authentication.Scram qualified as Scram
import Client.Configuration.ServerSettings
import Client.Hook (MessageHook)
import Client.Hooks (messageHooks)
import Client.Network.Async (abortConnection, send, NetworkConnection, TerminationReason(PingTimeout))
import Client.State.Channel
import Client.UserHost (UserAndHost(UserAndHost, _uhAccount))
import Client.WhoReply
import Control.Lens
import Data.Bits (Bits((.&.)))
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.Either qualified as Either
import Data.Foldable (for_, traverse_ )
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (foldl', delete, intersect, sort, sortBy, union)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Read qualified as Text
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Irc.Codes
import Irc.Commands
import Irc.Identifier (Identifier, idText, mkId)
import Irc.Message
import Irc.Modes
import Irc.RawIrcMsg
import Irc.UserInfo
import LensUtils (overStrict, setStrict)
import System.Random qualified as Random

-- | State tracked for each IRC connection
data NetworkState = NetworkState
  { _csChannels     :: !(HashMap Identifier ChannelState) -- ^ joined channels
  , _csChannelList  :: !ChannelList -- ^ cached ELIST parameter and /list output
  , _csWhoReply     :: !WhoReply -- ^ cached reply from the last WHO query
  , _csSocket       :: !NetworkConnection -- ^ network socket
  , _csModeTypes    :: !ModeTypes -- ^ channel mode meanings
  , _csUmodeTypes   :: !ModeTypes -- ^ user mode meanings
  , _csChannelTypes :: ![Char] -- ^ channel identifier prefixes
  , _csTransaction  :: !Transaction -- ^ state for multi-message sequences
  , _csModes        :: ![Char] -- ^ modes for the connected user
  , _csSnomask      :: ![Char] -- ^ server notice modes for the connected user
  , _csStatusMsg    :: ![Char] -- ^ modes that prefix statusmsg channel names
  , _csSettings     :: !ServerSettings -- ^ settings used for this connection
  , _csUserInfo     :: !UserInfo -- ^ usermask used by the server for this connection
  , _csUsers        :: !(HashMap Identifier UserAndHost) -- ^ user and hostname for other nicks
  , _csModeCount    :: !Int -- ^ maximum mode changes per MODE command
  , _csNetwork      :: !Text -- ^ name of network connection
  , _csMessageHooks :: ![MessageHook] -- ^ names of message hooks to apply to this connection
  , _csAuthenticationState :: !AuthenticateState
  , _csAway         :: !Bool -- ^ Tracks when you are marked away

  -- Timing information
  , _csNextPingTime :: !(Maybe UTCTime) -- ^ time for next ping event
  , _csLatency      :: !(Maybe NominalDiffTime) -- ^ latency calculated from previous pong
  , _csPingStatus   :: !PingStatus      -- ^ state of ping timer
  , _csLastReceived :: !(Maybe UTCTime) -- ^ time of last message received
  , _csCertificate  :: ![Text]

  -- Randomization
  , _csSeed         :: Random.StdGen
  }

-- | State of the authentication transaction
data AuthenticateState
  = AS_None               -- ^ no active transaction
  | AS_PlainStarted       -- ^ PLAIN mode initiated
  | AS_EcdsaStarted       -- ^ ECDSA-NIST mode initiated
  | AS_EcdsaWaitChallenge -- ^ ECDSA-NIST user sent waiting for challenge
  | AS_ExternalStarted    -- ^ EXTERNAL mode initiated
  | AS_ScramStarted
  | AS_Scram1 Scram.Phase1
  | AS_Scram2 Scram.Phase2
  | AS_EcdhStarted
  | AS_EcdhWaitChallenge Ecdh.Phase1

-- | Status of the ping timer
data PingStatus
  = PingSent !UTCTime -- ^ ping sent at given time, waiting for pong
  | PingNone          -- ^ not waiting for a pong
  | PingConnecting !Int !(Maybe UTCTime) !ConnectRestriction -- ^ number of attempts, last known connection time
  deriving Show

-- | Cached channel information from /list and elsewhere.
data ChannelList = ChannelList
  { _clsElist :: !(Maybe Text) -- ^ The last ELIST parameter used. Nothing is also used to trigger cache purges
  , _clsDone  :: !Bool -- ^ Whether to purge the hash map on receiving a new RPL_LIST
  , _clsItems :: ![(Identifier, Int, Text)] -- ^ The list of channel infos.
  }

data ConnectRestriction
  = NoRestriction       -- ^ no message restriction
  | StartTLSRestriction -- ^ STARTTLS hasn't finished
  | WaitTLSRestriction  -- ^ No messages allowed until TLS starts
  deriving Show

-- | Timer-based events
data TimedAction
  = TimedDisconnect    -- ^ terminate the connection due to timeout
  | TimedSendPing      -- ^ transmit a ping to the server
  | TimedForgetLatency -- ^ erase latency (when it is outdated)
  deriving (Eq, Ord, Show)

data Transaction
  = NoTransaction
  | NamesTransaction [Text]
  | BanTransaction [(Text,MaskListEntry)]
  | WhoTransaction [UserInfo]
  | CapLsTransaction [(Text, Maybe Text)]
  deriving Show


makeLenses ''NetworkState
makeLenses ''ChannelList
makePrisms ''Transaction
makePrisms ''PingStatus
makePrisms ''TimedAction

newChannelList :: Maybe Text -> Maybe (Identifier, Int, Text) -> ChannelList
newChannelList elist Nothing = ChannelList
  { _clsElist = elist
  , _clsDone = False
  , _clsItems = []
  }
newChannelList elist (Just v) = ChannelList
  { _clsElist = elist
  , _clsDone = False
  , _clsItems = [v]
  }

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

    actionPrefix = "\^AACTION "
    actionSuffix = "\^A"

    -- Special case for splitting a single CTCP ACTION into
    -- multiple actions
    multiline cmd tgt txt
      | Just txt1 <- Text.stripPrefix actionPrefix txt
      , Just txt2 <- Text.stripSuffix actionSuffix txt1 =
      let txtChunks     = utf8ChunksOf maxContentLen txt2
          maxContentLen = computeMaxMessageLength (view csUserInfo cs) tgt
                        - Text.length actionPrefix - Text.length actionSuffix
      in for_ txtChunks $ \txtChunk ->
           transmit $ rawIrcMsg cmd [tgt, actionPrefix <> txtChunk <> actionSuffix]

    -- Normal case
    multiline cmd tgt txt =
      let txtChunks     = utf8ChunksOf maxContentLen txt
          maxContentLen = computeMaxMessageLength (view csUserInfo cs) tgt
      in for_ txtChunks $ \txtChunk ->
           transmit $ rawIrcMsg cmd [tgt, txtChunk]

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
  Text              {- ^ network name              -} ->
  ServerSettings    {- ^ server settings           -} ->
  NetworkConnection {- ^ active network connection -} ->
  PingStatus        {- ^ initial ping status       -} ->
  Random.StdGen     {- ^ initial random seed       -} ->
  NetworkState      {- ^ new network state         -}
newNetworkState network settings sock ping seed = NetworkState
  { _csUserInfo     = UserInfo "*" "" ""
  , _csChannels     = HashMap.empty
  , _csChannelList  = newChannelList Nothing Nothing
  , _csWhoReply     = finishWhoReply $ newWhoReply "" ""
  , _csSocket       = sock
  , _csChannelTypes = defaultChannelTypes
  , _csModeTypes    = defaultModeTypes
  , _csUmodeTypes   = defaultUmodeTypes
  , _csTransaction  = NoTransaction
  , _csModes        = ""
  , _csSnomask      = ""
  , _csStatusMsg    = ""
  , _csSettings     = settings
  , _csModeCount    = 3
  , _csUsers        = HashMap.empty
  , _csNetwork      = network
  , _csMessageHooks = buildMessageHooks (view ssMessageHooks settings)
  , _csAuthenticationState = AS_None
  , _csAway         = False
  , _csPingStatus   = ping
  , _csLatency      = Nothing
  , _csNextPingTime = Nothing
  , _csLastReceived = Nothing
  , _csCertificate  = []
  , _csSeed         = seed
  }

buildMessageHooks :: [HookConfig] -> [MessageHook]
buildMessageHooks = mapMaybe \(HookConfig name args) ->
  do hookFun <- HashMap.lookup name messageHooks
     hookFun args

data Apply = Apply [RawIrcMsg] NetworkState

hideMessage :: IrcMsg -> Bool
hideMessage m =
  case m of
    Authenticate{} -> True
    BatchStart{} -> True
    BatchEnd{} -> True
    Ping{} -> True
    Pong{} -> True
    Reply _ RPL_WHOSPCRPL [_,"616",_,_,_,_] -> True
    _ -> False

-- | Used for updates to a 'NetworkState' that require no reply.
noReply :: NetworkState -> Apply
noReply = reply []

reply :: [RawIrcMsg] -> NetworkState -> Apply
reply = Apply

overChannel :: Identifier -> (ChannelState -> ChannelState) -> NetworkState -> NetworkState
overChannel chan = overStrict (csChannels . ix chan)

overChannels :: (ChannelState -> ChannelState) -> NetworkState -> NetworkState
overChannels = overStrict (csChannels . traverse)

applyMessage :: ZonedTime -> IrcMsg -> NetworkState -> Apply
applyMessage msgWhen msg cs
  = applyMessage' msgWhen msg
  $ set csLastReceived (Just $! zonedTimeToUTC msgWhen) cs

applyMessage' :: ZonedTime -> IrcMsg -> NetworkState -> Apply
applyMessage' msgWhen msg cs =
  case msg of
    Ping args -> reply [ircPong args] cs
    Pong _    -> noReply (doPong msgWhen cs)
    Join user chan acct _ ->
         reply response
         $ recordUser (srcUser user) acct
         $ overChannel chan (joinChannel (userNick (srcUser user)))
         $ createOnJoin (srcUser user) chan cs
     where
       response =
         [ircMode chan [] | userNick (srcUser user) == view csNick cs]

    Account user acct ->
           noReply
         $ recordUser (srcUser user) acct cs

    Chghost user newUser newHost ->
           noReply
         $ updateUserInfo (userNick (srcUser user)) newUser newHost cs

    Quit user _reason ->
           noReply
         $ forgetUser (userNick (srcUser user))
         $ overChannels (partChannel (userNick (srcUser user))) cs

    Part user chan _mbreason -> exitChannel chan (userNick (srcUser user))

    Kick _kicker chan nick _reason -> exitChannel chan nick

    Nick oldNick newNick ->
         let nick = userNick (srcUser oldNick) in
           noReply
         $ renameUser nick newNick
         $ updateMyNick nick newNick
         $ overChannels (nickChange nick newNick) cs

    Reply _ RPL_WELCOME (me:_) -> doWelcome msgWhen (mkId me) cs
    Reply _ RPL_SASLSUCCESS _ -> reply [ircCapEnd] cs
    Reply _ ERR_SASLFAIL _ -> reply [ircCapEnd] cs
    Reply _ ERR_SASLABORTED _ -> reply [ircCapEnd] cs
    Reply _ RPL_SASLMECHS _ -> reply [ircCapEnd] cs

    Reply _ ERR_NICKNAMEINUSE (_:badnick:_)
      | PingConnecting{} <- view csPingStatus cs -> doBadNick badnick cs
    Reply _ ERR_BANNEDNICK (_:badnick:_)
      | PingConnecting{} <- view csPingStatus cs -> doBadNick badnick cs
    Reply _ ERR_ERRONEUSNICKNAME (_:badnick:_)
      | PingConnecting{} <- view csPingStatus cs -> doBadNick badnick cs
    Reply _ ERR_UNAVAILRESOURCE (_:badnick:_)
      | PingConnecting{} <- view csPingStatus cs -> doBadNick badnick cs

    Reply _ RPL_HOSTHIDDEN (_:host:_) ->
        noReply (set (csUserInfo . uiHost) host cs)

    -- /who <#channel> %tuhna,616
    -- TODO: Use a different magic token here?
    Reply _ RPL_WHOSPCRPL [_me,"616",user,host,nick,acct] ->
       let acct' = if acct == "0" then "*" else acct
       in noReply (recordUser (UserInfo (mkId nick) user host) acct' cs)

    Reply _ code args      -> doRpl code msgWhen args cs
    Cap cmd                -> doCap cmd cs
    Authenticate param     -> doAuthenticate param cs
    Mode who target (modes:params) -> doMode msgWhen (srcUser who) target modes params cs
    Topic user chan topic  -> noReply (doTopic msgWhen (srcUser user) chan topic cs)
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
  NetworkState ->
  Apply
doWelcome msgWhen me
  = noReply
  . set csNick me
  . set csNextPingTime (Just $! addUTCTime 30 (zonedTimeToUTC msgWhen))
  . set csPingStatus PingNone

-- | Handle 'ERR_NICKNAMEINUSE' errors when connecting.
doBadNick ::
  Text {- ^ bad nickname -} ->
  NetworkState ->
  Apply
doBadNick badNick cs =
  case NonEmpty.dropWhile (badNick/=) (view (csSettings . ssNicks) cs) of
    _:next:_ -> reply [ircNick next] cs
    _        -> doRandomNick cs

-- | Pick a random nickname now that we've run out of choices
doRandomNick :: NetworkState -> Apply
doRandomNick cs = reply [ircNick candidate] cs'
  where
    limit       = 9 -- RFC 2812 puts the maximum nickname length as low as 9!
    range       = (0, 99999::Int) -- up to 5 random digits
    suffix      = show n
    primaryNick = NonEmpty.head (view (csSettings . ssNicks) cs)
    candidate   = Text.take (limit-length suffix) primaryNick <> Text.pack suffix

    (n, cs')    = cs & csSeed %%~ Random.randomR range

doList :: [Text] -> NetworkState -> NetworkState
doList (_:chan:users:topic) cs
  | purge = set csChannelList (newChannelList elist (Just $! value)) cs
  | otherwise = set (csChannelList . clsItems) items' cs
  where
    items' = value:(_clsItems . _csChannelList $ cs)
    value = (mkId chan, usercount, fromMaybe "" (listToMaybe topic))
    usercount = fst . Either.fromRight (0, "") . Text.decimal $ users
    elist = _clsElist . _csChannelList $ cs
    purge = _clsDone . _csChannelList $ cs
doList _ cs = cs

doListEnd :: NetworkState -> NetworkState
doListEnd cs = set csChannelList ncl cs
  where
    ncl = ChannelList { _clsElist = elist, _clsDone = True, _clsItems = sorted }
    elist = _clsElist . _csChannelList $ cs
    sorted = sortBy sorter . _clsItems . _csChannelList $ cs
    sorter (_, aU, _) (_, bU, _) = compare bU aU

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

doRpl :: ReplyCode -> ZonedTime -> [Text] -> NetworkState -> Apply
doRpl cmd msgWhen args cs =
  case cmd of
    RPL_UMODEIS ->
      case args of
        _me:modes:params
          | Just xs <- splitModes (view csUmodeTypes cs) modes params ->
                 noReply
               $ doMyModes xs
               $ set csModes "" cs -- reset modes
        _ -> noReply cs

    RPL_SNOMASK ->
      case args of
        _me:snomask0:_
          | Just snomask <- Text.stripPrefix "+" snomask0 ->
           noReply (set csSnomask (Text.unpack snomask) cs)
        _ -> noReply cs

    RPL_NOTOPIC ->
      case args of
        _me:chan:_ -> noReply
                    $ overChannel
                        (mkId chan)
                        (setTopic "" . set chanTopicProvenance Nothing)
                        cs
        _ -> noReply cs

    RPL_TOPIC ->
      case args of
        _me:chan:topic:_ -> noReply (overChannel (mkId chan) (setTopic topic) cs)
        _                -> noReply cs

    RPL_TOPICWHOTIME ->
      case args of
        _me:chan:who:whenTxt:_ | Just when <- parseTimeParam whenTxt ->
          let !prov = TopicProvenance
                       { _topicAuthor = parseUserInfo who
                       , _topicTime   = when
                       }
          in noReply (overChannel (mkId chan) (set chanTopicProvenance (Just prov)) cs)
        _ -> noReply cs

    RPL_CREATIONTIME ->
      case args of
        _me:chan:whenTxt:_ | Just when <- parseTimeParam whenTxt ->
          noReply (overChannel (mkId chan) (set chanCreation (Just when)) cs)
        _ -> noReply cs

    RPL_CHANNEL_URL ->
      case args of
        _me:chan:urlTxt:_ ->
          noReply (overChannel (mkId chan) (set chanUrl (Just urlTxt)) cs)
        _ -> noReply cs

    RPL_MYINFO -> noReply (myinfo args cs)

    RPL_ISUPPORT -> noReply (isupport args cs)

    RPL_NAMREPLY ->
      case args of
        _me:_sym:_tgt:x:_ ->
           noReply $
           over csTransaction
                (\t -> let xs = view _NamesTransaction t
                       in xs `seq` NamesTransaction (x:xs))
                cs
        _ -> noReply cs

    RPL_ENDOFNAMES ->
      case args of
        _me:tgt:_ -> noReply (loadNamesList (mkId tgt) cs)
        _         -> noReply cs

    RPL_BANLIST ->
      case args of
        _me:_tgt:mask:who:whenTxt:_ -> noReply (recordListEntry mask who whenTxt cs)
        _                           -> noReply cs

    RPL_ENDOFBANLIST ->
      case args of
        _me:tgt:_ -> noReply (saveList 'b' tgt cs)
        _         -> noReply cs

    RPL_QUIETLIST ->
      case args of
        _me:_tgt:_q:mask:who:whenTxt:_ -> noReply (recordListEntry mask who whenTxt cs)
        _                              -> noReply cs

    RPL_ENDOFQUIETLIST ->
      case args of
        _me:tgt:_ -> noReply (saveList 'q' tgt cs)
        _         -> noReply cs

    RPL_INVEXLIST ->
      case args of
        _me:_tgt:mask:who:whenTxt:_ -> noReply (recordListEntry mask who whenTxt cs)
        _                           -> noReply cs

    RPL_ENDOFINVEXLIST ->
      case args of
        _me:tgt:_ -> noReply (saveList 'I' tgt cs)
        _         -> noReply cs

    RPL_EXCEPTLIST ->
      case args of
        _me:_tgt:mask:who:whenTxt:_ -> noReply (recordListEntry mask who whenTxt cs)
        _                           -> noReply cs

    RPL_ENDOFEXCEPTLIST ->
      case args of
        _me:tgt:_ -> noReply (saveList 'e' tgt cs)
        _         -> noReply cs

    RPL_WHOREPLY ->
      case args of
        _me:_tgt:uname:host:_server:nick:_ ->
          noReply $
          over csWhoReply (recordWhoReply args) $
          over csTransaction (\t ->
            let !x  = UserInfo (mkId nick) uname host
                !xs = view _WhoTransaction t
            in WhoTransaction (x : xs))
            cs
        _ -> noReply cs
    RPL_WHOSPCRPL -> noReply (over csWhoReply (recordWhoXReply args) cs)
    RPL_ENDOFWHO -> noReply (over csWhoReply finishWhoReply $ massRegistration cs)

    RPL_CHANNELMODEIS ->
      case args of
        _me:chan:modes:params ->
              doMode msgWhen who chanId modes params
            $ set (csChannels . ix chanId . chanModes) Map.empty cs
            where chanId = mkId chan
                  !who = UserInfo "*" "" ""
        _ -> noReply cs

    -- Away flag tracking
    RPL_NOWAWAY -> noReply (set csAway True cs)
    RPL_UNAWAY  -> noReply (set csAway False cs)

    -- /list
    RPL_LIST    -> noReply (doList args cs)
    RPL_LISTEND -> noReply (doListEnd cs)

    _ -> noReply cs


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
    RPL_SNOMASK         -> True
    RPL_WHOREPLY        -> True
    RPL_ENDOFWHO        -> True
    RPL_WHOSPCRPL       -> True
    RPL_TOPICWHOTIME    -> True
    RPL_CREATIONTIME    -> True
    RPL_CHANNEL_URL     -> True
    RPL_NOTOPIC         -> True
    RPL_LISTSTART       -> True
    RPL_LIST            -> True
    RPL_LISTEND         -> True
    RPL_HELPSTART       -> True
    RPL_HELPTXT         -> True
    RPL_ENDOFHELP       -> True
    _                   -> False

-- | Return 'True' for messages that should be hidden outside of
-- full detail view. These messages are interpreted by the client
-- so the user shouldn't need to see them directly to get the
-- relevant information.
squelchIrcMsg :: IrcMsg -> Bool
squelchIrcMsg (Reply _ rpl _) = squelchReply rpl
squelchIrcMsg _               = False

doMode ::
  ZonedTime {- ^ time of message -} ->
  UserInfo  {- ^ sender          -} ->
  Identifier {- ^ channel        -} ->
  Text       {- ^ mode flags     -} ->
  [Text]     {- ^ mode parameters -} ->
  NetworkState ->
  Apply
doMode when who target modes args cs
  | view csNick cs == target
  , Just xs <- splitModes (view csUmodeTypes cs) modes args =
        noReply (doMyModes xs cs)

  | isChannelIdentifier cs target
  , Just xs <- splitModes (view csModeTypes cs) modes args
  , let cs' = doChannelModes when who target xs cs =

    if iHaveOp target cs'
      then let (response, cs_) = cs' & csChannels . ix target . chanQueuedModeration <<.~ []
           in reply response cs_
      else noReply cs'

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

selectCaps ::
  NetworkState         {- ^ network state  -} ->
  [(Text, Maybe Text)] {- ^ server caps    -} ->
  [Text]               {- ^ caps to enable -}
selectCaps cs offered = (supported `intersect` Map.keys capMap)
                        `union`
                        view (csSettings . ssCapabilities) cs
  where
    capMap = Map.fromList offered

    supported =
      sasl ++ serverTime ++
      ["multi-prefix", "batch", "znc.in/playback", "znc.in/self-message"
      , "cap-notify", "extended-join", "account-notify", "chghost"
      , "userhost-in-names", "account-tag", "solanum.chat/identify-msg"
      , "solanum.chat/realhost", "away-notify"]

    -- logic for using IRCv3.2 server-time if available and falling back
    -- to ZNC's specific extension otherwise.
    serverTime
      | "server-time"            `Map.member` capMap = ["server-time"]
      | "znc.in/server-time-iso" `Map.member` capMap = ["znc.in/server-time-iso"]
      | otherwise                                    = []

    ss = view csSettings cs
    sasl = ["sasl" | isJust (view ssSaslMechanism ss) ]

decodeAuthParam :: Text -> Maybe B.ByteString
decodeAuthParam "+" = Just ""
decodeAuthParam xs =
  case B64.decode (Text.encodeUtf8 xs) of
    Right bs -> Just bs
    Left _ -> Nothing

abortAuth :: NetworkState -> Apply
abortAuth = reply [ircAuthenticate "*"] . set csAuthenticationState AS_None

doAuthenticate :: Text -> NetworkState -> Apply
doAuthenticate paramTxt cs =
  case decodeAuthParam paramTxt of
    Nothing -> abortAuth cs
    Just param -> doAuthenticate' param cs

doAuthenticate' :: B.ByteString -> NetworkState -> Apply
doAuthenticate' param cs =
  case view csAuthenticationState cs of
    AS_PlainStarted
      | B.null param
      , Just (SaslPlain mbAuthz authc (SecretText pass)) <- view ssSaslMechanism ss
      , let authz = fromMaybe "" mbAuthz
      -> reply
           (ircAuthenticates (encodePlainAuthentication authz authc pass))
           (set csAuthenticationState AS_None cs)

    AS_ExternalStarted
      | B.null param
      , Just (SaslExternal mbAuthz) <- view ssSaslMechanism ss
      , let authz = fromMaybe "" mbAuthz
      -> reply
           (ircAuthenticates (encodeExternalAuthentication authz))
           (set csAuthenticationState AS_None cs)

    AS_EcdsaStarted
      | B.null param
      , Just (SaslEcdsa mbAuthz authc _) <- view ssSaslMechanism ss
      -> reply
           (ircAuthenticates (Ecdsa.encodeAuthentication mbAuthz authc))
           (set csAuthenticationState AS_EcdsaWaitChallenge cs)

    AS_EcdsaWaitChallenge -> noReply cs -- handled in Client.EventLoop!

    AS_ScramStarted
      | B.null param
      , Just (SaslScram digest mbAuthz user (SecretText pass))
          <- view ssSaslMechanism ss
      , let authz = fromMaybe "" mbAuthz
      , (nonce, cs') <- cs & csSeed %%~ scramNonce
      , (msg, scram1) <-
          Scram.initiateScram digest
            (Text.encodeUtf8 user)
            (Text.encodeUtf8 authz)
            (Text.encodeUtf8 pass)
            nonce
      -> reply
           (ircAuthenticates msg)
           (set csAuthenticationState (AS_Scram1 scram1) cs')

    AS_Scram1 scram1
      | Just (rsp, scram2) <- Scram.addServerFirst scram1 param
      -> reply
           (ircAuthenticates rsp)
           (set csAuthenticationState (AS_Scram2 scram2) cs)

    AS_Scram2 scram2
      | Scram.addServerFinal scram2 param
      -> reply
           [ircAuthenticate "+"]
           (set csAuthenticationState AS_None cs)

    AS_EcdhStarted
      | B.null param
      , Just (SaslEcdh mbAuthz authc (SecretText key)) <- view ssSaslMechanism ss
      , Just (rsp, ecdh1) <- Ecdh.clientFirst mbAuthz authc key
      -> reply
           (ircAuthenticates rsp)
           (set csAuthenticationState (AS_EcdhWaitChallenge ecdh1) cs)
    
    AS_EcdhWaitChallenge ecdh1
      | Just rsp <- Ecdh.clientResponse ecdh1 param
      -> reply (ircAuthenticates rsp) (set csAuthenticationState AS_None cs)

    _ -> abortAuth cs

  where
    ss = view csSettings cs

scramNonce :: Random.StdGen -> (B.ByteString, Random.StdGen)
scramNonce = go [] nonceSize
  where
    alphabet = "!\"#$%&'()*+-./0123456789:;<=>?@\
               \ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`\
               \abcdefghijklmnopqrstuvwxyz{|}~"

    nonceSize = 20 :: Int -- ceiling (128 / logBase 9 (length alphabet))

    go acc 0 g = (B.pack acc, g)
    go acc i g =
      case Random.randomR (0, B.length alphabet-1) g of
        (x,g') -> go (B.index alphabet x:acc) (i-1) g'

doCap :: CapCmd -> NetworkState -> Apply
doCap cmd cs =
  case cmd of
    (CapLs CapMore caps) ->
      noReply (set csTransaction (CapLsTransaction (caps ++ prevCaps)) cs)
      where
        prevCaps = view (csTransaction . _CapLsTransaction) cs

    CapLs CapDone caps
      | null reqCaps -> reply [ircCapEnd] cs'
      | otherwise    -> reply [ircCapReq reqCaps] cs'
      where
        reqCaps = selectCaps cs (caps ++ view (csTransaction . _CapLsTransaction) cs)
        cs' = set csTransaction NoTransaction cs

    CapNew caps
      | null reqCaps -> noReply cs
      | otherwise    -> reply [ircCapReq reqCaps] cs
      where
        reqCaps = selectCaps cs caps

    CapDel _ -> noReply cs

    CapAck caps
      | let ss = view csSettings cs
      , "sasl" `elem` caps
      , Just mech <- view ssSaslMechanism ss ->
        case mech of
          SaslEcdsa{} ->
            reply [ircAuthenticate Ecdsa.authenticationMode]
                  (set csAuthenticationState AS_EcdsaStarted cs)
          SaslPlain{} ->
            reply [ircAuthenticate "PLAIN"]
                  (set csAuthenticationState AS_PlainStarted cs)
          SaslExternal{} ->
            reply [ircAuthenticate "EXTERNAL"]
                  (set csAuthenticationState AS_ExternalStarted cs)
          SaslScram digest _ _ _ ->
            reply [ircAuthenticate (Scram.mechanismName digest)]
                  (set csAuthenticationState AS_ScramStarted cs)
          SaslEcdh{} ->
            reply [ircAuthenticate Ecdh.mechanismName]
                  (set csAuthenticationState AS_EcdhStarted cs)

    _ -> reply [ircCapEnd] cs

initialMessages :: NetworkState -> [RawIrcMsg]
initialMessages cs
   = [ ircCapLs ]
  ++ [ ircPass pass | Just (SecretText pass) <- [view ssPassword ss]]
  ++ [ ircNick (views ssNicks NonEmpty.head ss)
     , ircUser (view ssUser ss) (view ssReal ss)
     ]
  where
    ss = view csSettings cs

loadNamesList :: Identifier -> NetworkState -> NetworkState
loadNamesList chan cs
  = set csTransaction NoTransaction
  $ flip (foldl' (flip learnUserInfo)) (fst <$> entries)
  $ setStrict (csChannels . ix chan . chanUsers) newChanUsers
  $ cs
  where
    newChanUsers = HashMap.fromList [ (view uiNick ui, modes) | (ui, modes) <- entries ]

    -- userhost-in-names might or might not include the user and host
    -- if we find it we update the user information.
    learnUserInfo (UserInfo n u h)
      | Text.null u || Text.null h = id
      | otherwise = updateUserInfo n u h

    sigils = toListOf (csModeTypes . modesPrefixModes . folded . _2) cs

    splitEntry modes str
      | Text.head str `elem` sigils = splitEntry (Text.head str : modes)
                                                 (Text.tail str)
      | otherwise = (parseUserInfo str, reverse modes)

    entries :: [(UserInfo, [Char])]
    entries = fmap (splitEntry "")
            $ concatMap Text.words
            $ view (csTransaction . _NamesTransaction) cs


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

myinfo ::
  [Text] ->
  NetworkState ->
  NetworkState
myinfo (_me : _host : _version : umodes : _) =
  -- special logic for s because I know it has arguments
  set (csUmodeTypes . modesNeverArg) (delete 's' (Text.unpack umodes))
myinfo _ = id

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

recordUser :: UserInfo -> Text -> NetworkState -> NetworkState
recordUser (UserInfo nick user host) acct
  | Text.null user || Text.null host = id
  | otherwise = set (csUsers . at nick)
                    (Just $! UserAndHost user host acct)

-- | Process a CHGHOST command, updating a users information
updateUserInfo ::
  Identifier {- ^ nickname     -} ->
  Text       {- ^ new username -} ->
  Text       {- ^ new hostname -} ->
  NetworkState -> NetworkState
updateUserInfo nick user host =
  over (csUsers . at nick) $ \old ->
    Just $! UserAndHost user host (maybe "" _uhAccount old)

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
            HashMap.alter
                (\mb -> case mb of
                          Nothing                     -> Just $! UserAndHost user host ""
                          Just (UserAndHost _ _ acct) -> Just $! UserAndHost user host acct
                ) nick users
      | otherwise = users

-- | Compute the earliest timed action for a connection, if any
nextTimedAction :: NetworkState -> Maybe (UTCTime, TimedAction)
nextTimedAction ns = minimumOf (folded.folded) actions
  where
    actions = [nextPingAction ns, nextForgetAction ns]

-- | Compute the timed action for forgetting the ping latency.
-- The client will wait for a multiple of the current latency
-- for the next pong response in order to reduce jitter in
-- the rendered latency when everything is fine.
nextForgetAction :: NetworkState -> Maybe (UTCTime, TimedAction)
nextForgetAction ns =
  do sentAt  <- preview (csPingStatus . _PingSent) ns
     latency <- view csLatency ns
     let delay = max 0.1 (3 * latency) -- wait at least 0.1s (ensure positive waits)
         eventAt = addUTCTime delay sentAt
     return (eventAt, TimedForgetLatency)

-- | Compute the next action needed for the client ping logic.
nextPingAction :: NetworkState -> Maybe (UTCTime, TimedAction)
nextPingAction cs =
  do runAt <- view csNextPingTime cs
     return (runAt, action)
  where
    action =
      case view csPingStatus cs of
        PingSent sentAt
          | Just sentAt < view csLastReceived cs -> TimedSendPing
          | otherwise -> TimedDisconnect
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
    TimedForgetLatency ->
      do return $! set csLatency Nothing cs

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
