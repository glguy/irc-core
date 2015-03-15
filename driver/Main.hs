{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Applicative hiding ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Char
import Data.Foldable (for_, traverse_)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Traversable (for)
import Graphics.Vty
import Network
import System.IO
import System.IO.Error (isEOFError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Irc.Core
import Irc.Cmd
import Irc.Format
import Irc.Message
import Irc.Model
import Irc.RateLimit

import ClientState
import CommandArgs
import CommandParser
import CtcpHandler
import ImageUtils
import Views.BanList
import Views.Channel
import Views.ChannelInfo
import HaskellHighlighter (highlightType)
import qualified EditBox as Edit

data SendType = SendCtcp String | SendPriv | SendNotice | SendAction
makePrisms ''SendType

main :: IO ()
main = do
  args <- getCommandArgs

  h    <- connectTo (view cmdArgServer args) (PortNumber (fromIntegral (view cmdArgPort args)))
  hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
  hSetEncoding h utf8

  hErr <- for (view cmdArgDebug args) $ \fn ->
            do hErr <- openFile fn WriteMode
               hSetBuffering hErr NoBuffering
               return hErr

  let toId = mkId . B8.pack

  initializeConnection args h

  vtyEventChan <- atomically newTChan
  socketChan   <- atomically newTChan
  sendChan     <- atomically newTChan

  let conn0 = defaultIrcConnection
                { _connNick = views cmdArgNick toId args
                , _connSasl = fmap (\p -> (views cmdArgSaslUser B8.pack args,B8.pack p))
                                   (view cmdArgSaslPass args)
                }

  cfg <- standardIOConfig
  bracket (mkVty cfg) shutdown $ \vty ->
    do _ <- forkIO (socketLoop socketChan h hErr)
       _ <- forkIO (vtyEventLoop vtyEventChan vty)
       _ <- forkIO (sendLoop sendChan h)
       (width,height) <- displayBounds (outputIface vty)
       driver vty vtyEventChan socketChan ClientState
         { _clientSendChan        = sendChan
         , _clientErrors          = hErr
         , _clientConnection      = conn0
         , _clientFocus           = ChannelFocus ""
         , _clientDetailView      = False
         , _clientTimeView        = True
         , _clientMetaView        = True
         , _clientEditBox         = Edit.empty
         , _clientTabPattern      = Nothing
         , _clientScrollPos       = 0
         , _clientHeight          = height
         , _clientWidth           = width
         , _clientIgnores         = mempty
         , _clientHighlights      = mempty
         , _clientMessages        = mempty
         , _clientNickColors      = defaultNickColors
         , _clientAutomation      = [ctcpHandler]
         }

initializeConnection :: CommandArgs -> Handle -> IO ()
initializeConnection args h =
  do B.hPut h capLsCmd
     traverse_ (B.hPut h . passCmd . toUtf8) (view cmdArgPassword args)
     B.hPut h (nickCmd (views cmdArgNick toId args))
     B.hPut h (userCmd (views cmdArgUser toUtf8 args)
                       (views cmdArgReal toUtf8 args))
  where

      toUtf8 = Text.encodeUtf8 . Text.pack
      toId = mkId . B8.pack

driver :: Vty -> TChan Event -> TChan (UTCTime, MsgFromServer) -> ClientState -> IO ()
driver vty vtyEventChan ircMsgChan st0 =
  do let (scroll', pic) = picForState st0
         st1 = set clientScrollPos scroll' st0
     update vty pic
     e <- readEitherTChan vtyEventChan ircMsgChan
     case e of
       Left vtyEvent -> processVtyEvent st1 vtyEvent
       Right (time,msg) -> processIrcMsg st1 time msg

  where
  continue = driver vty vtyEventChan ircMsgChan
           . resetCurrentChannelMessages

  processVtyEvent st event =
    case event of
      EvResize width height -> continue
                             $ set clientWidth  width
                             $ set clientHeight height st

      EvKey (KChar 'l') [MCtrl] ->
        do refresh vty
           (width,height) <- displayBounds (outputIface vty)
           continue $ set clientHeight height
                    $ set clientWidth  width st

      EvKey key mods -> do r <- keyEvent key mods st
                           case r of
                             KeepGoing st' -> continue st'
                             Exit -> return ()

      _ -> continue st

  processIrcMsg st time msg =
    do let m :: IO (Either String IrcConnection, ClientState)
           m = flip runStateT st
             $ retract
             $ hoistFree (interpretLogicOp ircMsgChan)
             $ runLogic time (advanceModel msg (view clientConnection st))

       res <- m
       case res of
         (Left e,st') -> do for_ (view clientErrors st) $ \h -> hPutStrLn h ("!!! " ++ e)
                            continue st'
         (Right conn',st') -> continue (set clientConnection conn' st')

interpretLogicOp :: TChan (UTCTime, MsgFromServer) -> LogicOp a -> StateT ClientState IO a

interpretLogicOp ircMsgChan (Expect k) =
  do fmap (k.snd) (liftIO (atomically (readTChan ircMsgChan)))

interpretLogicOp _ (Emit bytes r) =
  do st <- get
     liftIO (clientSend bytes st)
     return r

interpretLogicOp _ (Record target message r) =
  do put =<< liftIO . runEventHandlers target message
                    . addMessage target message
         =<< get
     return r

------------------------------------------------------------------------
-- Key Event Handlers!
------------------------------------------------------------------------

data EventResult
  = KeepGoing ClientState
  | Exit

changeInput :: (Edit.EditBox -> Edit.EditBox) -> ClientState -> ClientState
changeInput f st = clearTabPattern (over clientEditBox f st)

inputLogic :: ClientState -> (Image, IO EventResult)
inputLogic st =
  case clientInput st of
    '/':_ -> case commandEvent st of
               (img, Nothing) -> (img, return (KeepGoing st))
               (img, Just m ) -> (img, m)
    txt -> (stringWithControls defAttr txt, fmap KeepGoing (doSendMessageCurrent SendPriv st))

keyEvent :: Key -> [Modifier] -> ClientState -> IO EventResult
keyEvent k ms st =
  let more = return . KeepGoing in
  case (k,ms) of
    (KFun 2   , []     ) -> more $ over clientDetailView not st
    (KFun 3   , []     ) -> more $ over clientTimeView   not st
    (KFun 4   , []     ) -> more $ over clientMetaView   not st
    (KPageUp  , _      ) -> more $ scrollUp st
    (KPageDown, _      ) -> more $ scrollDown st
    (KChar 'n', [MCtrl]) -> more $ nextFocus st
    (KChar 'p', [MCtrl]) -> more $ prevFocus st
    (KBS      , _      ) -> more $ changeInput Edit.backspace st
    (KChar 'd', [MCtrl]) -> more $ changeInput Edit.delete st
    (KDel     , _      ) -> more $ changeInput Edit.delete st
    (KUp      , _      ) -> more $ maybe st clearTabPattern $ clientEditBox Edit.earlier st
    (KDown    , _      ) -> more $ maybe st clearTabPattern $ clientEditBox Edit.later st
    (KLeft    , _      ) -> more $ changeInput Edit.left st
    (KRight   , _      ) -> more $ changeInput Edit.right st
    (KHome    , _      ) -> more $ changeInput Edit.home st
    (KEnd     , _      ) -> more $ changeInput Edit.end st
    (KChar 'a', [MCtrl]) -> more $ changeInput Edit.home st
    (KChar 'e', [MCtrl]) -> more $ changeInput Edit.end st
    (KChar 'u', [MCtrl]) -> more $ changeInput Edit.killHome st
    (KChar 'k', [MCtrl]) -> more $ changeInput Edit.killEnd st
    (KChar 'w', [MCtrl]) -> more $ changeInput Edit.killWord st
    (KChar 'b', [MMeta]) -> more $ changeInput Edit.leftWord st
    (KChar 'f', [MMeta]) -> more $ changeInput Edit.rightWord st
    (KChar '\t', []    ) -> more $ tabComplete st
    (KChar 'b', [MCtrl]) -> more $ changeInput (Edit.insert '\^B') st
    (KChar 'c', [MCtrl]) -> more $ changeInput (Edit.insert '\^C') st
    (KChar ']', [MCtrl]) -> more $ changeInput (Edit.insert '\^]') st
    (KChar '_', [MCtrl]) -> more $ changeInput (Edit.insert '\^_') st
    (KChar 'v', [MCtrl]) -> more $ changeInput (Edit.insert '\^V') st
    (KChar c  , []     ) -> more $ changeInput (Edit.insert c) st
    (KEnter   , []     ) -> snd (inputLogic st)
    _                    -> more st


-- TODO: Don't scroll off the end of the channel

scrollOffset :: ClientState -> Int
scrollOffset st = max 1 (view clientHeight st - 4)

scrollUp :: ClientState -> ClientState
scrollUp st = over clientScrollPos (+ scrollOffset st) st

scrollDown :: ClientState -> ClientState
scrollDown st = over clientScrollPos (\x -> max 0 (x - scrollOffset st)) st

doSendMessageCurrent :: SendType -> ClientState -> IO ClientState
doSendMessageCurrent sendType st =
  case view clientFocus st of
    ChannelFocus c
      | not (null (dropWhile isSpace (clientInput st))) ->
      doSendMessage sendType c (Text.pack (clientInput st)) st
    _ -> return st


doSendMessage :: SendType -> Identifier -> Text -> ClientState -> IO ClientState

doSendMessage sendType _ message st
  | Text.null message && hasn't _SendCtcp sendType = return st

doSendMessage sendType target message st =
  do let bs = case sendType of
                SendPriv -> privMsgCmd target (Text.encodeUtf8 message)
                SendAction -> privMsgCmd target ("\SOHACTION " <>
                                                 Text.encodeUtf8 message <> "\SOH")
                SendNotice -> noticeCmd target (Text.encodeUtf8 message)
                SendCtcp cmd -> ctcpRequestCmd target
                                 (Text.encodeUtf8 (Text.pack (map toUpper cmd)))
                                 (Text.encodeUtf8 message)
     clientSend bs st
     now <- getCurrentTime
     let myNick = view connNick conn
         myModes = view (connChannels . ix target' . chanUsers . ix myNick) conn
     return (addMessage target' (fakeMsg now myModes) (clearInput st))

  where
  conn = view clientConnection st

  (statusmsg, target') = splitStatusMsg target conn

  fakeMsg now modes = IrcMessage
    { _mesgSender = who
    , _mesgStatus = statusmsg
    , _mesgType = case sendType of
                    SendPriv   -> PrivMsgType   message
                    SendNotice -> NoticeMsgType message
                    SendAction -> ActionMsgType message
                    SendCtcp cmd -> CtcpReqMsgType (Text.encodeUtf8 (Text.pack cmd))
                                                   (Text.encodeUtf8 message)
    , _mesgStamp = now
    , _mesgModes = modes
    , _mesgMe = True
    }

  who = UserInfo (view (clientConnection . connNick) st)
                 Nothing
                 Nothing

commandEvent :: ClientState -> (Image, Maybe (IO EventResult))
commandEvent st = commandsParser (clientInput st)
                        (exitCommand : normalCommands)
 where
 st' = clearInput st
 toB = Text.encodeUtf8 . Text.pack

 exitCommand =
  ("exit", pure (return Exit))

 normalCommands = over (mapped . _2 . mapped . mapped) KeepGoing $
    -- focus setting
  [ ("server",
    pure (return (set clientFocus (ChannelFocus "") st')))

  , ("channel",
    (\chan -> return (set clientFocus (ChannelFocus chan) st'))
    <$> pChannel st)

  , ("query",
    (\user -> return (set clientFocus (ChannelFocus user) st'))
    <$> pNick st)

  , ("channelinfo", pure (doChannelInfoCmd st'))

  , ("bans", pure (doMasksCmd 'b' st))

  , ("masks",
    (\mode -> doMasksCmd mode st)
    <$> pValidToken "mode" (\m ->
            case m of
              [x] | x `elem` view (clientConnection . connChanModeTypes . modesLists) st -> Just x
              _ -> Nothing))

    -- chat
  , ("me",
    (\msg -> doAction msg st)
    <$> pRemainingNoSp)

  , ("notice",
    (\target msg -> doSendMessage SendNotice target (Text.pack msg) st)
    <$> pTarget <*> pRemainingNoSp)

  , ("msg",
    (\target msg -> doSendMessage SendPriv target (Text.pack msg) st)
    <$> pTarget <*> pRemainingNoSp)

  , ("ctcp",
    (\command params -> doSendMessage (SendCtcp command) (focusedName st) (Text.pack params) st)
    <$> pToken "command" <*> pRemainingNoSp)

    -- carefully preserve whitespace after the command
  , ("hs",
    (\src ->
      let msg = Text.pack src in
      case view clientFocus st of
        ChannelFocus c -> doSendMessage SendPriv c msg st
        _ -> return st)
    <$> pHaskell)

  , ("type",
    (\msg ->
      case highlightType msg of
        Nothing -> return st
        Just x  ->
          case view clientFocus st of
            ChannelFocus c -> doSendMessage SendPriv c (Text.pack x) st
            _ -> return st)
    <$> pRemainingNoSp)

    -- raw
  , ("quote",
    (\rest -> doQuote rest st')
    <$> pRemainingNoSp)

  , ("help",
    (\mbTopic -> st' <$ clientSend (helpCmd (maybe "" toB mbTopic)) st')
    <$> optional (pToken "topic"))

    -- channel commands
  , ("join",
    (\c key -> doJoinCmd c (fmap toB key) st')
    <$> pChannel st <*> optional (pToken "key"))

  , ("umode",
    (\args ->
         st' <$ clientSend (modeCmd (view (clientConnection . connNick) st)
                                    (map toB (words args))) st')
    <$> pRemainingNoSp)

  , ("mode",
    (\args -> doMode (Text.encodeUtf8 (Text.pack args)) st)
    <$> pRemainingNoSp)

  , ("kick",
    (\nick msg ->
       case focusedChan st of
         Nothing -> return st
         Just chan ->
           doWithOps chan (\evSt ->
             evSt <$ clientSend (kickCmd chan nick (toB msg)) evSt) st')
    <$> pNick st <*> pRemainingNoSp)

  , ("remove",
    (\nick msg ->
        case focusedChan st of
          Nothing -> return st
          Just chan ->
            doWithOps chan (\evSt ->
              evSt <$ clientSend (removeCmd chan nick (toB msg)) evSt) st')
    <$> pNick st <*> pRemainingNoSp)

  , ("invite",
    (\nick ->
       case focusedChan st of
         Nothing -> return st
         Just chan ->
           doInvite nick chan st)
    <$> pNick st)

  , ("knock",
    (\chan -> doKnock chan st)
    <$> pChannel st)

  , ("part",
    (\msg -> case focusedChan st of
               Nothing -> return st
               Just chan -> st' <$ clientSend (partCmd chan (toB msg)) st')
    <$> pRemainingNoSp)

  , ("whois",
    (\u -> st' <$ clientSend (whoisCmd u) st')
    <$> pNick st)

  , ("whowas",
    (\u -> st' <$ clientSend (whowasCmd u) st')
    <$> pNick st)

  , ("topic",
    (\rest ->
        case focusedChan st of
          Nothing -> return st
          Just chan -> doTopicCmd chan (toB rest) st')
    <$> pRemainingNoSp)

  , ("ignore",
    (\u -> return (over (clientIgnores . contains u) not st'))
    <$> pNick st)

  , ("highlight",
    (\w ->
       return (over (clientHighlights . contains (ircFoldCase (Text.encodeUtf8 (Text.pack w)))) not st'))
    <$> pRemainingNoSp)

  , ("clear",
    (\chan -> return (set (clientMessages . at (fromMaybe (focusedName st) chan)) Nothing st'))
    <$> optional pTarget)

  , ("accept",
    (\mbNick -> doAccept True mbNick st)
    <$> optional (pNick st))

  , ("unaccept",
    (\mbNick -> doAccept False mbNick st)
    <$> optional (pNick st))

  , ("acceptlist", pure (doAccept True (Just "*") st))

  , ("nick",
    (\nick -> st' <$ clientSend (nickCmd nick) st')
    <$> pNick st)

  , ("away",
    (\rest -> st' <$ clientSend (awayCmd (toB rest)) st')
    <$> pRemainingNoSp)

  , ("quit",
    (\rest -> st' <$ clientSend (quitCmd (toB rest)) st')
    <$> pRemainingNoSp)

  , ("who",
    (\whomask -> st' <$ clientSend (whoCmd (toB whomask)) st')
    <$> pToken "mask")

  , ("op",
    (\args ->
       case focusedChan st of
        Nothing -> return st
        Just chan ->
          doChanservOpCmd chan (map B8.pack (words args)) st')
    <$> pRemainingNoSp)

  , ("akb",
    (\nick reason ->
       case focusedChan st of
         Nothing -> return st
         Just chan -> doWithOps chan (doAutoKickBan chan nick (Text.pack reason)) st')
    <$> pNick st <*> pRemainingNoSp)

  , ("time",
    (\mbServer -> st' <$ clientSend (timeCmd (fmap (Text.encodeUtf8 . Text.pack) mbServer)) st')
    <$> optional (pToken "server"))

  ]

doMode ::
  ByteString {- mode change -} ->
  ClientState -> IO ClientState
doMode args st = fromMaybe (return st) $
  do chan         <- focusedChan st
     modes:params <- Just (B8.words args)
     parsedModes  <- splitModes (view (clientConnection.connChanModeTypes) st)
                        modes params
     let modeChunks = chunksOf (view (clientConnection.connModes) st) parsedModes
     return $
       doWithOps chan (\evSt ->
         do for_ modeChunks $ \modeChunk ->
              clientSend (modeCmd chan (unsplitModes modeChunk)) evSt
            return evSt)
         (clearInput st)

doAccept ::
  Bool {- ^ add to list -} ->
  Maybe Identifier {- ^ optional nick -} ->
  ClientState -> IO ClientState
doAccept add mbNick st =
  case mbNick of
    Just n -> go n
    Nothing
      | isNickName (focusedName st) (view clientConnection st) -> go (focusedName st)
      | otherwise -> return st
  where
  go nick =
    do let nickBytes
             | add = idBytes nick
             | otherwise = "-" <> idBytes nick
       clientSend (acceptCmd nickBytes) st
       return (clearInput st)

doAction ::
  String {- ^ action text -} ->
  ClientState -> IO ClientState
doAction msg st =
  case view clientFocus st of
    ChannelFocus c -> doSendMessage SendAction c (Text.pack msg) (clearInput st)
    _ -> return st

doKnock ::
  Identifier {- ^ channel  -} ->
  ClientState -> IO ClientState
doKnock chan st
  | not available || not isChannel || inChannel = return st
  | otherwise = do clientSend (knockCmd chan) st
                   return (clearInput st)
  where
  available = view (clientConnection . connKnock) st
  inChannel = has (clientConnection . connChannels . ix chan) st
  isChannel = isChannelName chan (view clientConnection st)

doInvite ::
  Identifier {- ^ nickname -} ->
  Identifier {- ^ channel  -} ->
  ClientState -> IO ClientState
doInvite nick chan st

  -- 'g' is the "FREEINVITE" mode, don't check for ops
  | has (clientConnection . connChannels . ix chan . chanModes . folded . ix 'g') st = go (clearInput st)

  -- it's an error to invite someone already in channel
  | has (clientConnection . connChannels . ix chan . chanUsers . ix nick) st = return st

  | otherwise = doWithOps chan go (clearInput st)
  where
  go st' = st' <$ clientSend (inviteCmd nick chan) st'

doChanservOpCmd ::
  Identifier   {- ^ channel -} ->
  [ByteString] {- ^ optional arguments -} ->
  ClientState -> IO ClientState
doChanservOpCmd chan args st =
  do clientSend (privMsgCmd (mkId "chanserv")
                            (B8.unwords ("op":idBytes chan:args))) st
     return st

doChannelInfoCmd ::
  ClientState -> IO ClientState
doChannelInfoCmd st

  | Just chan <- focusedChan st =

  do let modesKnown
           = has ( clientConnection
           . connChannels . ix chan
           . chanModes
           . folded
           ) st
     unless modesKnown $
       clientSend (modeCmd chan []) st
     return (clearInput (set clientFocus (ChannelInfoFocus chan) st))

  | otherwise = return st

doMasksCmd ::
  Char       {- ^ mode    -} ->
  ClientState -> IO ClientState
doMasksCmd mode st
  | Just chan <- focusedChan st =
  do let masksKnown =
           has ( clientConnection
               . connChannels . ix chan
               . chanMaskLists
               . ix mode
               ) st
     unless masksKnown $
       clientSend (modeCmd chan [B8.pack ['+',mode]]) st
     return (set clientFocus (MaskListFocus mode chan) (clearInput st))

  | otherwise = return st

doTopicCmd ::
  Identifier {- ^ channel -} ->
  ByteString {- ^ new topic -} ->
  ClientState -> IO ClientState
doTopicCmd chan topic st =
  case preview (clientConnection . connChannels . ix chan . chanModes . folded) st of
    -- check if it's known that the mode isn't +t
    Just modes | hasn't (ix 't') modes -> go st
    _                                  -> doWithOps chan go st
  where
  go st' = st' <$ clientSend (topicCmd chan topic) st'

doJoinCmd :: Identifier -> Maybe ByteString -> ClientState -> IO ClientState
doJoinCmd c mbKey st =
  do clientSend (joinCmd c mbKey) st
     return (set clientFocus (ChannelFocus c) st)

doQuote :: String -> ClientState -> IO ClientState
doQuote cmd st = st <$ clientSend (Text.encodeUtf8 (Text.pack (cmd ++ "\r\n"))) st


------------------------------------------------------------------------
-- Primary UI rendering
------------------------------------------------------------------------

picForState :: ClientState -> (Int,Picture)
picForState st = (scroll', pic)
  where
  pic = Picture
    { picCursor = Cursor (min (view clientWidth st - 1)
                              (view (clientEditBox. Edit.pos) st+1))
                         (view clientHeight st - 1)
    , picLayers =
        [ translateY (view clientHeight st - 2)
             (divider <-> textbox st)
        , everythingBeforeInput
        ]
    , picBackground = ClearBackground
    }

  everythingBeforeInput = vertCat
    [ titlebar
    , string defAttr (replicate (view clientWidth st) '─')
    , mainFocusImage
    ]

  divider = dividerImage st

  -- Pad the main image when it doesn't fill the screen
  -- so that it starts at the bottom of the frame
  startFromBottom :: Image -> Image
  startFromBottom img = pad 0 top 0 0 img
    where
    top = max 0 (view clientHeight st - 4 - imageHeight img)

  mainFocusImage
    = startFromBottom
    . vertCat
    . reverse
    $ scrolledLines

  (scroll', scrolledLines) = scrollList (view clientHeight st - 4)
                                        (view clientScrollPos st)
                                        wrappedLines

  wrappedLines = reverse . lineWrap (view clientWidth st) =<< mainFocusLines

  mainFocusLines =
    case view clientFocus st of
      MaskListFocus mode chan -> banListImage mode chan st
      ChannelInfoFocus chan -> channelInfoImage chan st
      _ -> channelImage st

  titlebar =
    case view clientFocus st of
      ChannelFocus "" -> string defAttr "Server"
      ChannelFocus c -> topicbar c
      ChannelInfoFocus c -> string defAttr "Channel Info: "
                        <|> identImg defAttr c
      MaskListFocus mode c -> string defAttr (maskListTitle mode ++ ": ")
                          <|> identImg defAttr c

  topicbar chan =
    case preview (clientConnection . connChannels . ix chan . chanTopic . folded . folded . _1) st of
      Just topic | not (Text.null topic) -> text' (withForeColor defAttr green) topic
      _ -> char defAttr ' '

-- | Try to drop the suggested number of elements and then
-- take the requested number of elements. If the drop drops
-- too many, drop fewer (when possible) in favor of taking
-- the correct number of elements. Return the actual number
-- of elements dropped along with the elements kept.
scrollList ::
  Int {- ^ take parameter -} ->
  Int {- ^ suggested drop -} ->
  [a] {- ^ all elements   -} ->
  (Int,[a]) {- ^ (actual drop, selected elements) -}
scrollList t d xs
  | length xs' == t = (d,xs')
  | otherwise       = (d',drop d' xs)
  where
  xs' = take t (drop d xs)
  x'len = length xs'
  d' = max 0 (d - (t - x'len))

maskListTitle :: Char -> String
maskListTitle 'b' = "Bans"
maskListTitle 'q' = "Quiets"
maskListTitle 'I' = "Invite exceptions"
maskListTitle 'e' = "Ban exceptions"
maskListTitle m   = "Unknown '" ++ [m] ++ "' masks"

textbox :: ClientState -> Image
textbox st
  = applyCrop
  $ beginning <|> content <|> ending
  where
  pos = view (clientEditBox . Edit.pos) st
  width = view clientWidth st
  (content,_) = inputLogic st
  applyCrop
    | 1+pos < width = cropRight width
    | otherwise     = cropLeft  width . cropRight (pos+2)

  beginning = char (withForeColor defAttr brightBlack) '^'
  ending    = char (withForeColor defAttr brightBlack) '$'

dividerImage :: ClientState -> Image
dividerImage st
  = extendToWidth (nickPart <|> channelPart)
  where
  conn = view clientConnection st

  myNick = view connNick conn

  channelPart =
    ifoldr (\i x xs -> drawOne i x <|> xs)
           emptyImage
           (fullMessageLists st <> extraDefaults)

  -- e.g. glguy(+Zi)
  nickPart =
    identImg defAttr myNick <|>
    string defAttr "(+" <|>
    utf8Bytestring' defAttr (view connUmode conn) <|>
    char defAttr ')'

  drawOne :: Identifier -> MessageList -> Image
  drawOne i seen
    | focusedName st == i =
        string defAttr "─(" <|>
        string (withForeColor defAttr blue) focusedChanPrefixes <|>
        utf8Bytestring'
          (withForeColor defAttr green)
          (identToBytes i) <|>
        string defAttr ")"
    | views mlNewMessages (>0) seen =
        string defAttr "─[" <|>
        utf8Bytestring'
          (withForeColor defAttr brightBlue)
          (identToBytes i) <|>
        string defAttr ":" <|>
        string (withForeColor defAttr (seenColor seen))
               (show (view mlNewMessages seen)) <|>
        string defAttr "]"
    | otherwise =
        string defAttr "─o"

  -- e.g. [('o','@'),('v','+')]
  prefixMapping = view (connChanModeTypes . modesPrefixModes) conn
  myFocusedModes = view (connChannels . ix (focusedName st) . chanUsers . ix myNick) conn

  -- allow prefixMapping to dictate the ordering
  focusedChanPrefixes = [ prefix | (mode,prefix) <- prefixMapping, mode `elem` myFocusedModes ]

  -- deal with the fact that the server window uses the "" identifier
  identToBytes x
    | x == "" = "server"
    | otherwise = idBytes x

  seenColor :: MessageList -> Color
  seenColor seen
    | view mlMentioned seen = red
    | view mlNewMessages seen > 0 = green
    | otherwise = brightBlack

  extendToWidth img =
    img <|> string defAttr (replicate (view clientWidth st - imageWidth img) '─')

  extraDefaults =
    Map.singleton (focusedName st) defaultMessageList

------------------------------------------------------------------------
-- Event loops
------------------------------------------------------------------------

sendLoop :: TChan ByteString -> Handle -> IO ()
sendLoop queue h =
  do r <- newRateLimit 2 5
     forever $
       do x <- atomically (readTChan queue)
          tickRateLimit r
          B.hPut h x

socketLoop :: TChan (UTCTime, MsgFromServer) -> Handle -> Maybe Handle -> IO ()
socketLoop chan h hErr =
  forever (atomically . writeTChan chan =<< getOne h hErr)
  `catches`
   [ Handler $ \ioe ->
      do let msg = if isEOFError ioe
                    then "Connection terminated"
                    else Text.encodeUtf8 (Text.pack (show ioe))
         now <- getCurrentTime
         atomically (writeTChan chan (now, Error msg))
   , Handler $ \(SomeException e) ->
        do now <- getCurrentTime
           atomically (writeTChan chan (now, Error (Text.encodeUtf8 (Text.pack (show e)))))
   ]

vtyEventLoop :: TChan Event -> Vty -> IO a
vtyEventLoop chan vty = forever (atomically . writeTChan chan =<< nextEvent vty)

getOne :: Handle -> Maybe Handle -> IO (UTCTime, MsgFromServer)
getOne h hErr =
    do xs <- ircGetLine h
       case parseRawIrcMsg xs of
         Nothing -> debug xs >> getOne h hErr
         Just msg ->
           case ircMsgToServerMsg msg of
             Nothing -> debug msg >> getOne h hErr
             Just x -> do t <- case msgTime msg of
                                 Nothing -> getCurrentTime
                                 Just t  -> return t
                          debug x
                          return (t,x)
  where
  debug x = for_ hErr (`hPrint` x)

readEitherTChan :: TChan a -> TChan b -> IO (Either a b)
readEitherTChan a b =
  atomically (fmap Left (readTChan a) `orElse` fmap Right (readTChan b))

------------------------------------------------------------------------
-- Tab completion logic
------------------------------------------------------------------------

tabComplete :: ClientState -> ClientState
tabComplete st = fromMaybe st $
  do let current = currentWord st
     guard (not (null current))
     c <- focusedChan st
     let vals = userSet c <> channelSet
     case view clientTabPattern st of
       Just pat -> do next <- tabSearch pat current vals
                      Just $ replaceWith next st

       Nothing  -> do next <- tabSearch current current vals
                      Just $ set clientTabPattern (Just current)
                           $ replaceWith next st
  where
  replaceWith str = over clientEditBox $ \box ->
    let box1 = Edit.killWord box
        str1 | view Edit.pos box1 == 0 = str ++ ": "
             | otherwise               = str
    in Edit.insertString str1 box1

  userSet c  = views (clientConnection . connChannels . ix c . chanUsers) Map.keysSet st
  channelSet = views (clientConnection . connChannels)                    Map.keysSet st

currentWord :: ClientState -> String
currentWord st
  = reverse
  $ takeWhile (not . isSpace)
  $ dropWhile (\x -> x==' ' || x==':')
  $ reverse
  $ take (view (clientEditBox . Edit.pos) st) (clientInput st)

tabSearch :: String -> String -> Set Identifier -> Maybe String
tabSearch pat cur vals
  | Just next <- Set.lookupGT cur' vals
  , B.isPrefixOf (idDenote pat') (idDenote next)
  = Just (B8.unpack (idBytes next))

  -- wrap around when pat is a user
  | Just next <- Set.lookupGE pat' vals
  , B.isPrefixOf (idDenote pat') (idDenote next)
  = Just (B8.unpack (idBytes next)) -- TODO: Use original case

  -- if all else fails, do nothing
  | otherwise = Nothing
  where
  pat' = mkId (B8.pack pat)
  cur' = mkId (B8.pack cur)

defaultNickColors :: [Color]
defaultNickColors =
  [cyan, magenta, green, yellow, blue,
   brightCyan, brightMagenta, brightGreen, brightBlue]


-- | Perform a privileged operation. If the connection doesn't
-- already have +o on the channel it will be requested from
-- ChanServ and the privileged operation will be scheduled to
-- run when the connection gets +o.
doWithOps ::
  Identifier {- ^ channel -} ->
  (ClientState -> IO ClientState) {- ^ privileged operation -} ->
  ClientState -> IO ClientState
doWithOps chan privop st
    | alreadyOp = finishUp False st
    | otherwise = getOpFirst

  where
  conn = view clientConnection st
  myNick = view connNick conn

  alreadyOp =
    elemOf ( connChannels . ix chan
           . chanUsers . ix myNick
           . folded)
           'o'
           conn

  handler = EventHandler
    { _evName = "Get op for privop"
    , _evOnEvent = \evTgt evMsg evSt ->
         case view mesgType evMsg of
           ModeMsgType True 'o' modeNick
             | mkId modeNick == myNick
             , evTgt    == chan -> finishUp True evSt
           _ -> return (over clientAutomation (cons handler) evSt)
    }

  finishUp deop st1 =
    do st2 <- privop st1
       when deop $
         clientSend (modeCmd chan ["-o",idDenote myNick]) st2
       return st2

  getOpFirst =
    do clientSend (privMsgCmd "chanserv" ("op " <> idDenote chan)) st
       return (over clientAutomation (cons handler) st)

doAutoKickBan ::
  Identifier {- ^ channel -} ->
  Identifier {- ^ nick    -} ->
  Text       {- ^ reason  -} ->
  ClientState -> IO ClientState
doAutoKickBan chan nick reason st =
  -- TODO: Look up account name or hostname!
  do clientSend (modeCmd chan ["+b",banMask]) st
     clientSend (kickCmd chan nick (Text.encodeUtf8 reason)) st
     return st

  where
  usr  = view (clientConnection . connUsers . at nick) st
  nickMask = idDenote nick <> "!*@*"
  banMask = fromMaybe nickMask
          $ previews (folded . usrAccount . folded) ("$a:"<>) usr
    `mplus` previews (folded . usrHost    . folded) ("*!*@"<>) usr
