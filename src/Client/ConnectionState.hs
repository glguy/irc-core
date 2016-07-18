{-# Language TemplateHaskell, OverloadedStrings #-}
module Client.ConnectionState where

import           Client.ChannelState
import           Client.NetworkConnection
import           Client.ServerSettings
import           Control.Lens
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import           Data.Time
import           Data.Time.Clock.POSIX
import           Irc.Codes
import           Irc.Identifier
import           Irc.Message
import           Irc.Modes
import           Irc.RawIrcMsg (RawIrcMsg, rawIrcMsg, renderRawIrcMsg)
import           Irc.UserInfo
import           LensUtils

data ConnectionState = ConnectionState
  { _csChannels     :: !(HashMap Identifier ChannelState)
  , _csNick         :: !Identifier
  , _csSocket       :: !NetworkConnection
  , _csModeTypes    :: !ModeTypes
  , _csChannelTypes :: ![Char]
  , _csTransaction  :: !Transaction
  , _csModes        :: ![Char]
  , _csStatusMsg    :: ![Char]
  , _csSettings     :: !ServerSettings
  }
  deriving Show

data Transaction
  = NoTransaction
  | WhoTransaction [Text]
  deriving Show

makeLenses ''ConnectionState

sendMsg :: NetworkConnection -> RawIrcMsg -> IO ()
sendMsg c msg = send c (renderRawIrcMsg msg)

newConnectionState ::
  ServerSettings ->
  NetworkConnection ->
  ConnectionState
newConnectionState settings sock = ConnectionState
  { _csNick         = mkId (view ssNick settings)
  , _csChannels     = HashMap.empty
  , _csSocket       = sock
  , _csChannelTypes = "#&"
  , _csModeTypes    = defaultModeTypes
  , _csTransaction  = NoTransaction
  , _csModes        = ""
  , _csStatusMsg    = ""
  , _csSettings     = settings
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
    Ping args -> ([pongMsg args], cs)
    Join user chan ->
           noReply
         $ overChannel chan (joinChannel (userNick user))
         $ createOnJoin (userNick user) chan cs

    Quit user _reason ->
           noReply
         $ overChannels (partChannel (userNick user)) cs

    Part user chan _mbreason ->
           noReply
         $ if userNick user == view csNick cs
             then set (csChannels . at chan) Nothing cs
             else overChannel chan (partChannel (userNick user)) cs

    Nick oldNick newNick ->
           noReply
         $ updateMyNick (userNick oldNick) newNick
         $ overChannels (nickChange (userNick oldNick) newNick) cs

    Kick _kicker chan nick _reason ->
           noReply (overChannel chan (partChannel nick) cs)
    Reply code args        -> noReply (doRpl code args cs)
    Cap cmd params         -> doCap cmd params cs
    Mode _ target (modes:params)  -> noReply (doMode target modes params cs)
    Topic user chan topic  -> noReply (doTopic msgWhen user chan topic cs)
    _                      -> noReply cs

doTopic :: ZonedTime -> UserInfo -> Identifier -> Text -> ConnectionState -> ConnectionState
doTopic when user chan topic =
  overChannel chan (setTopic topic . set chanTopicProvenance (Just $! prov))
  where
    prov = TopicProvenance
             { _topicAuthor = user
             , _topicTime   = zonedTimeToUTC when
             }

doRpl :: Int -> [Text] -> ConnectionState -> ConnectionState
doRpl RPL_NOTOPIC (_me:chan:_) =
  overChannel (mkId chan) (setTopic "" . set chanTopicProvenance Nothing)
doRpl RPL_TOPIC [_me,chan,topic] = overChannel (mkId chan) (setTopic topic)
doRpl RPL_TOPICWHOTIME [_me,chan,who,whenTxt]
  | Right (whenSecs, "") <- Text.decimal whenTxt =
    let prov = TopicProvenance
                 { _topicAuthor = parseUserInfo who
                 , _topicTime = posixSecondsToUTCTime (fromInteger whenSecs)
                 }
    in setStrict (csChannels . ix (mkId chan) . chanTopicProvenance) (Just $! prov)
doRpl RPL_ISUPPORT params = isupport params
doRpl RPL_NAMREPLY [_me,_sym,_tgt,x] =
           over csTransaction
                (\t -> let xs = currentWhoList t
                       in xs `seq` WhoTransaction (x:xs))
doRpl RPL_ENDOFNAMES [_me,tgt,_txt] = loadWhoList (mkId tgt)
doRpl RPL_CHANNELMODEIS (_me:chan:modes:args)
  = doMode chanId modes args
  . set (csChannels . ix chanId . chanModes) Map.empty
  where chanId = mkId chan

doRpl _ _ = id


doMode :: Identifier -> Text -> [Text] -> ConnectionState -> ConnectionState
doMode target modes args cs
  | view csNick cs == target
  , Just xs <- splitModes defaultUmodeTypes modes args =
        doMyModes xs cs

  | isChannelIdentifier cs target
  , Just xs <- splitModes (view csModeTypes cs) modes args =
        doChannelModes target xs cs

doMode _ _ _ cs = cs -- ignore bad mode command

doChannelModes :: Identifier -> [(Bool, Char, Text)] -> ConnectionState -> ConnectionState
doChannelModes chan changes cs = overChannel chan applyChannelModes cs
  where
    modeTypes = view csModeTypes cs
    sigilMap  = view modesPrefixModes modeTypes
    listModes = view modesLists modeTypes

    applyChannelModes c = foldl' applyChannelMode c changes

    applyChannelMode c (polarity, mode, arg)

      | Just sigil <- lookup mode sigilMap =
          over (chanUsers . ix (mkId arg))
               (setPrefixMode polarity sigil)
               c

      | mode `elem` listModes =
        setStrict (chanList mode . contains arg) polarity c

      | polarity  = set (chanModes . at mode) (Just arg) c
      | otherwise = set (chanModes . at mode) Nothing    c

    setPrefixMode polarity sigil sigils
      | not polarity        = delete sigil sigils
      | sigil `elem` sigils = sigils
      | otherwise           = filter (`elem` sigils') (map snd sigilMap)
      where
        sigils' = sigil : sigils


doMyModes :: [(Bool, Char, Text)] -> ConnectionState -> ConnectionState
doMyModes changes cs = over csModes (\modes -> foldl applyOne modes changes) cs
  where
    applyOne modes (True, mode, _)
      | mode `elem` modes = modes
      | otherwise         = mode:modes
    applyOne modes (False, mode, _) = delete mode modes

supportedCaps :: [Text]
supportedCaps = ["multi-prefix","znc.in/server-time-iso"]

doCap :: CapCmd -> [Text] -> ConnectionState -> ([RawIrcMsg], ConnectionState)
doCap cmd args cs =
  case (cmd,args) of
    (CapLs,[capsTxt])
      | null reqCaps -> (initialMessages cs, cs)
      | otherwise -> ([capReqMsg reqCaps], cs)
      where
        caps = Text.words capsTxt
        reqCaps = intersect supportedCaps caps

    (CapAck,_) -> (initialMessages cs, cs)
    (CapNak,_) -> (initialMessages cs, cs)

    _ -> noReply cs


initialMessages :: ConnectionState -> [RawIrcMsg]
initialMessages cs
   = [ capEndMsg ]
  ++ [ passMsg pass | Just pass <- [view ssPassword ss]]
  ++ [ userMsg (view ssUser ss) False True (view ssReal ss)
     , nickMsg (view csNick cs)
     ]
  where
    ss = view csSettings cs

passMsg :: Text -> RawIrcMsg
passMsg pass = rawIrcMsg "PASS" [pass]

pongMsg :: [Text] -> RawIrcMsg
pongMsg = rawIrcMsg "PONG"

userMsg ::
  Text {- ^ username -} ->
  Bool {- ^ set +w   -} ->
  Bool {- ^ set +i   -} ->
  Text {- ^ realname -} -> RawIrcMsg
userMsg user set_w set_i real = rawIrcMsg "USER" [user, modeTxt, "*", real]
  where
    modeTxt = Text.pack (show mode)
    mode :: Int
    mode = (if set_w then 4 else 0) -- bit 2
         + (if set_i then 8 else 0) -- bit 3

nickMsg :: Identifier -> RawIrcMsg
nickMsg nick = rawIrcMsg "NICK" [idText nick]

capReqMsg :: [Text] -> RawIrcMsg
capReqMsg caps = rawIrcMsg "CAP" ["REQ", Text.unwords caps]

capEndMsg :: RawIrcMsg
capEndMsg = rawIrcMsg "CAP" ["END"]

capLsMsg :: RawIrcMsg
capLsMsg = rawIrcMsg "CAP" ["LS"]

currentWhoList :: Transaction -> [Text]
currentWhoList t =
  case t of
    WhoTransaction xs -> xs
    _                 -> []

loadWhoList :: Identifier -> ConnectionState -> ConnectionState
loadWhoList chan cs
  = set csTransaction NoTransaction
  $ setStrict (csChannels . ix chan . chanUsers) newChanUsers
  $ cs
  where
    newChanUsers = HashMap.fromList (splitEntry "" <$> whoEntries)

    sigils = toListOf (csModeTypes . modesPrefixModes . folded . _2) cs

    splitEntry modes str
      | Text.head str `elem` sigils = splitEntry (Text.head str : modes)
                                                 (Text.tail str)
      | otherwise = (mkId str, reverse modes)

    whoEntries = concatMap Text.words (views csTransaction currentWhoList cs)


createOnJoin :: Identifier -> Identifier -> ConnectionState -> ConnectionState
createOnJoin user chan cs
  | user == view csNick cs =
        set (csChannels . at chan) (Just newChannel) cs
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
