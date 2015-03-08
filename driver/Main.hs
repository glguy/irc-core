{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

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
import Data.Foldable (traverse_)
import Data.Functor
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Graphics.Vty
import Network
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Irc.Core
import Irc.Cmd
import Irc.Format
import Irc.Model
import Irc.RateLimit

import ImageUtils
import CommandArgs
import HaskellHighlighter
import ClientState
import Views.Channel
import Views.ChannelInfo
import Views.BanList
import qualified EditBox as Edit

main :: IO ()
main = do
  args <- getCommandArgs

  h    <- connectTo (view cmdArgServer args) (PortNumber (fromIntegral (view cmdArgPort args)))
  hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
  hSetEncoding h utf8

  hErr <- openFile "debug.txt" WriteMode
  hSetBuffering hErr NoBuffering

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
         , _clientEditBox         = Edit.empty
         , _clientTabPattern      = Nothing
         , _clientScrollPos       = 0
         , _clientHeight          = height
         , _clientWidth           = width
         , _clientIgnores         = mempty
         , _clientHighlights      = mempty
         , _clientMessages        = mempty
         , _clientNickColors      = defaultNickColors
         , _clientAutomation      = []
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

driver :: Vty -> TChan Event -> TChan MsgFromServer -> ClientState -> IO ()
driver vty vtyEventChan ircMsgChan st0 =
  do let (scroll', pic) = picForState st0
         st1 = set clientScrollPos scroll' st0
     update vty pic
     e <- readEitherTChan vtyEventChan ircMsgChan
     case e of
       Left vtyEvent -> processVtyEvent st1 vtyEvent
       Right msg     -> processIrcMsg st1 msg

  where
  continue = driver vty vtyEventChan ircMsgChan
           . resetCurrentChannelMessages

  processVtyEvent st event =
    case event of
      -- quit
      EvKey KEsc _ -> return ()
      EvResize width height -> continue
                             $ set clientWidth  width
                             $ set clientHeight height st

      EvKey (KChar 'l') [MCtrl] ->
        do refresh vty
           (width,height) <- displayBounds (outputIface vty)
           continue $ set clientHeight height
                    $ set clientWidth  width st

      EvKey key mods -> continue =<< keyEvent key mods st

      _ -> continue st

  processIrcMsg st msg =
    do now <- getCurrentTime

       let m :: IO (Either String IrcConnection, ClientState)
           m = flip runStateT st
             $ retract
             $ hoistFree (interpretLogicOp ircMsgChan)
             $ runLogic now (advanceModel msg (view clientConnection st))

       res <- m
       case res of
         (Left e,st') -> do hPutStrLn (view clientErrors st) ("!!! " ++ e)
                            continue st'
         (Right conn',st') -> continue (set clientConnection conn' st')

interpretLogicOp :: TChan MsgFromServer -> LogicOp a -> StateT ClientState IO a

interpretLogicOp ircMsgChan (Expect k) =
  do fmap k (liftIO (atomically (readTChan ircMsgChan)))

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

keyEvent :: Key -> [Modifier] -> ClientState -> IO ClientState
keyEvent (KFun 2)    []      st = return $ over clientDetailView not st
keyEvent KPageUp     _       st = return $ scrollUp st
keyEvent KPageDown   _       st = return $ scrollDown st
keyEvent (KChar 'n') [MCtrl] st = return $ nextFocus st
keyEvent (KChar 'p') [MCtrl] st = return $ prevFocus st
keyEvent KBS         _       st = return $ clearTabPattern $ over clientEditBox Edit.backspace st
keyEvent (KChar 'd') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.delete st
keyEvent KDel        _       st = return $ clearTabPattern $ over clientEditBox Edit.delete st
keyEvent KUp         _       st = return $ maybe st clearTabPattern $ clientEditBox Edit.earlier st
keyEvent KDown       _       st = return $ maybe st clearTabPattern $ clientEditBox Edit.later st
keyEvent KLeft       _       st = return $ clearTabPattern $ over clientEditBox Edit.left st
keyEvent KRight      _       st = return $ clearTabPattern $ over clientEditBox Edit.right st
keyEvent KHome       _       st = return $ clearTabPattern $ over clientEditBox Edit.home st
keyEvent KEnd        _       st = return $ clearTabPattern $ over clientEditBox Edit.end st
keyEvent (KChar 'a') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.home st
keyEvent (KChar 'e') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.end st
keyEvent (KChar 'u') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.killHome st
keyEvent (KChar 'k') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.killEnd st
keyEvent (KChar 'w') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.killWord st
keyEvent (KChar 'b') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.left st
keyEvent (KChar 'f') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.right st
keyEvent (KChar 'b') [MMeta] st = return $ clearTabPattern $ over clientEditBox Edit.leftWord st
keyEvent (KChar 'f') [MMeta] st = return $ clearTabPattern $ over clientEditBox Edit.rightWord st
keyEvent (KChar '\t') []     st = return $ tabComplete st
keyEvent (KChar c)   []      st = return $ clearTabPattern $ over clientEditBox (Edit.insert c) st
keyEvent KEnter      []      st = case clientInput st of
                                    []          -> return st
                                    '/':command -> commandEvent command st
                                    _           -> doSendMessageCurrent SendPriv st
keyEvent _           _       st = return st


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
    ChannelFocus c ->
      doSendMessage
        sendType
        c
        (Text.pack (clientInput st))
        (clearInput st)
    _ -> return st

data SendType = SendPriv | SendNotice | SendAction

doSendMessage :: SendType -> Identifier -> Text -> ClientState -> IO ClientState
doSendMessage sendType target message st =
  do let bs = case sendType of
                SendPriv -> privMsgCmd target (Text.encodeUtf8 message)
                SendAction -> privMsgCmd target ("\SOHACTION " <>
                                                 Text.encodeUtf8 message <> "\SOH")
                SendNotice -> noticeCmd target (Text.encodeUtf8 message)
     clientSend bs st
     now <- getCurrentTime
     let myNick = view (clientConnection . connNick) st
         myModes = view (clientConnection . connChannels . ix target . chanUsers . ix myNick) st
     return (addMessage target (fakeMsg now myModes) st)

  where
  fakeMsg now modes = IrcMessage
    { _mesgSender = who
    , _mesgType = case sendType of
                    SendPriv   -> PrivMsgType   message
                    SendNotice -> NoticeMsgType message
                    SendAction -> ActionMsgType message
    , _mesgStamp = now
    , _mesgModes = modes
    , _mesgMe = True
    }

  who = UserInfo (view (clientConnection . connNick) st)
                 Nothing
                 Nothing


pattern (:-) arg rest <- (splitArg -> Just (arg,rest))
infixr 5 :-

splitArg :: String -> Maybe (String,String)
splitArg xs
  | null a = Nothing
  | otherwise = Just (a, dropWhile isSpace b)
  where
  (a,b) = break isSpace xs


commandEvent :: String -> ClientState -> IO ClientState
commandEvent cmd st =
  case cmd of

    -- focus setting
    "server" :- ""  ->
      return (set clientFocus (ChannelFocus "") st')

    "query"  :- user :- "" ->
      return (set clientFocus (ChannelFocus (toId user)) st')

    "channel" :- chan :- "" ->
      return (set clientFocus (ChannelFocus (toId chan)) st')

    "channelinfo" :- "" | Just chan <- focusedChan st ->
      doChannelInfoCmd chan st'

    "bans" :- "" | Just chan <- focusedChan st ->
      doMasksCmd chan 'b' st'

    "masks" :- [mode] :- ""
      | mode `elem` view (clientConnection . connChanModeTypes . modesLists) st
      , Just chan <- focusedChan st ->
        doMasksCmd chan mode st'

    -- chat
    "me" :- msg ->
      case view clientFocus st of
        ChannelFocus c -> doSendMessage SendAction c (Text.pack msg) st'
        _ -> return st
    "notice" :- target :- msg ->
      doSendMessage SendNotice (toId target) (Text.pack msg) st'
    "msg" :- target :- msg ->
      doSendMessage SendPriv (toId target) (Text.pack msg) st'

    -- carefully preserve whitespace after the command
    'h':'s':' ':rest ->
      case view clientFocus st of
        ChannelFocus c -> doSendMessage SendPriv c msg st'
        _ -> return st
       where
       msg = Text.pack (highlightHaskell rest)

    -- raw
    "quote" :- rest -> doQuote rest st'

    "help" :- topic :- "" -> st' <$ clientSend (helpCmd (B8.pack topic)) st'

    -- channel commands
    "join" :- c :-      "" -> doJoinCmd (toB c) Nothing st'
    "join" :- c :- k :- "" -> doJoinCmd (toB c) (Just (toB k)) st'

    "umode" :- args ->
         st' <$ clientSend (modeCmd (view (clientConnection . connNick) st)
                                    (map toB (words args))) st'

    "mode" :- args | Just chan <- focusedChan st ->
       doWithOps chan (\evSt ->
         evSt <$ clientSend (modeCmd chan (map toB (words args))) evSt) st'

    "kick" :- nick :- msg | Just chan <- focusedChan st ->
       doWithOps chan (\evSt ->
         evSt <$ clientSend (kickCmd chan (toId nick) (toB msg)) evSt) st'

    "remove" :- nick :- msg | Just chan <- focusedChan st ->
       doWithOps chan (\evSt ->
         evSt <$ clientSend (removeCmd chan (toId nick) (toB msg)) evSt) st'

    "invite" :- nick :- "" | Just chan <- focusedChan st ->
       doWithOps chan (\evSt ->
         evSt <$ clientSend (inviteCmd (toId nick) chan) evSt) st'

    "part" :- msg | Just chan <- focusedChan st ->
         st' <$ clientSend (partCmd chan (toB msg)) st'

    "whois"  :- u :- "" -> st' <$ clientSend (whoisCmd  (toId u)) st'
    "whowas" :- u :- "" -> st' <$ clientSend (whowasCmd (toId u)) st'

    "topic" :- rest | Just chan <- focusedChan st -> doTopicCmd chan (toB rest) st'

    "ignore" :- u :- "" -> return (over (clientIgnores . contains (toId u)) not st')
    "highlight" :- w :- "" ->
       return (over (clientHighlights . contains (CI.foldCase (Text.pack w))) not st')

    "clear" :- "" ->
         return (set (clientMessages . at (focusedName st)) Nothing st')

    "nick" :- nick :- "" -> st' <$ clientSend (nickCmd (toId nick)) st'

    "away" :- rest -> st' <$ clientSend (awayCmd (toB rest)) st'
    "quit" :- rest -> st' <$ clientSend (quitCmd (toB rest)) st'

    "list" :- rest -> st' <$ clientSend (listCmd (map toId (words rest))) st'
    "who" :- whomask :- "" -> st' <$ clientSend (whoCmd (toB whomask)) st'

    "op" :- args | Just chan <- focusedChan st ->
        doChanservOpCmd chan (map B8.pack (words args)) st'

    "akb" :- nick :- reason | Just chan <- focusedChan st ->
       doWithOps chan (doAutoKickBan chan (toId nick) (Text.pack reason)) st'

    _ -> return st

  where
  st' = clearInput st
  toB = Text.encodeUtf8 . Text.pack
  toId = mkId . toB

doChanservOpCmd ::
  Identifier   {- ^ channel -} ->
  [ByteString] {- ^ optional arguments -} ->
  ClientState -> IO ClientState
doChanservOpCmd chan args st =
  do clientSend (privMsgCmd (mkId "chanserv")
                            (B8.unwords ("op":idBytes chan:args))) st
     return st

doChannelInfoCmd ::
  Identifier {- ^ channel -} ->
  ClientState -> IO ClientState
doChannelInfoCmd chan st =
  do unless modesKnown $
       clientSend (modeCmd chan []) st
     return (set clientFocus (ChannelInfoFocus chan) st)
  where
  modesKnown = has ( clientConnection
                   . connChannels . ix chan
                   . chanModes
                   . folded
                   ) st

doMasksCmd ::
  Identifier {- ^ channel -} ->
  Char       {- ^ mode    -} ->
  ClientState -> IO ClientState
doMasksCmd chan mode st =
  do unless masksKnown $
       clientSend (modeCmd chan [B8.pack ['+',mode]]) st
     return (set clientFocus (MaskListFocus mode chan) st)
  where
  masksKnown = has ( clientConnection
                   . connChannels . ix chan
                   . chanMaskLists
                   . ix mode
                   ) st

doTopicCmd :: Identifier -> ByteString -> ClientState -> IO ClientState
doTopicCmd chan topic st =
 do st <$ clientSend (topicCmd chan topic) st

doJoinCmd :: ByteString -> Maybe ByteString -> ClientState -> IO ClientState
doJoinCmd c mbKey st =
  do clientSend (joinCmd c mbKey) st
     let c0 = mkId (B8.takeWhile (/=',') c) -- , separates channels
     return (set clientFocus (ChannelFocus c0) st)

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
             (divider <->
              textbox (clientInput st)
                      (view (clientEditBox. Edit.pos) st)
                      (view clientWidth st))
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
      _ | view clientDetailView st -> detailedImageForState st
        | otherwise                -> compressedImageForState st

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

textbox :: String -> Int -> Int -> Image
textbox str pos width
  = applyCrop
  $ beginning <|> string defAttr str <|> ending
  where
  applyCrop
    | pos < width = cropRight width
    | otherwise   = cropLeft  width . cropRight (pos+2)

  beginning = char (withForeColor defAttr brightBlack) '^'
  ending    = char (withForeColor defAttr brightBlack) '$'

dividerImage :: ClientState -> Image
dividerImage st
  = extendToWidth (nickPart <|> channelPart)
  where
  channelPart =
    ifoldr (\i x xs -> drawOne i x <|> xs)
           emptyImage
           (fullMessageLists st <> extraDefaults)

  nickPart =
    identImg defAttr (view (clientConnection . connNick) st) <|>
    string defAttr "(+" <|>
    utf8Bytestring' defAttr (view (clientConnection . connUmode) st) <|>
    char defAttr ')'

  drawOne :: Identifier -> MessageList -> Image
  drawOne i seen
    | focusedName st == i =
        string defAttr "─(" <|>
        utf8Bytestring'
          (withForeColor defAttr green)
          (identToBytes i) <|>
        string defAttr ")"
    | focusedName st == i =
        string defAttr "─(" <|>
        identImg (withForeColor defAttr green) i <|>
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

socketLoop :: TChan MsgFromServer -> Handle -> Handle -> IO a
socketLoop chan h hErr = forever (atomically . writeTChan chan =<< getOne h hErr)

vtyEventLoop :: TChan Event -> Vty -> IO a
vtyEventLoop chan vty = forever (atomically . writeTChan chan =<< nextEvent vty)

getOne :: Handle -> Handle -> IO MsgFromServer
getOne h hErr =
    do xs <- ircGetLine h
       case parseRawIrcMsg xs of
         Nothing -> hPrint hErr xs >> getOne h hErr
         Just msg ->
           case ircMsgToServerMsg msg of
             Just x -> hPrint hErr x >> return x
             Nothing -> hPrint hErr msg >> getOne h hErr

readEitherTChan :: TChan a -> TChan b -> IO (Either a b)
readEitherTChan a b =
  atomically (fmap Left (readTChan a) `orElse` fmap Right (readTChan b))

------------------------------------------------------------------------
-- Tab completion logic
------------------------------------------------------------------------

tabComplete :: ClientState -> ClientState
tabComplete st
  | null current = st
  | otherwise =
      case view clientFocus st of
        ChannelFocus c ->
          let users = Map.keysSet (view (clientConnection . connChannels . ix c . chanUsers) st)
          in
          case view clientTabPattern st of
            Just pat -> replaceWith (tabSearch pat current users) st
            Nothing  -> set clientTabPattern (Just current)
                      $ replaceWith (tabSearch current current users) st
        _ -> st
  where
  current = currentWord st
  replaceWith str = over clientEditBox (Edit.insertString str . Edit.killWord)

currentWord :: ClientState -> String
currentWord st
  = reverse
  $ takeWhile (not . isSpace)
  $ reverse
  $ take (view (clientEditBox . Edit.pos) st) (clientInput st)

tabSearch :: String -> String -> Set Identifier -> String
tabSearch pat cur users
  | not (Set.null b)
  , B.isPrefixOf (idDenote pat') (idDenote (Set.findMin b))
  = B8.unpack (idBytes (Set.findMin b))

  -- wrap around when pat is a user
  | Set.member pat' users
  = pat -- TODO: Use original case

  -- wrap around when pat is not a user
  | not (Set.null a')
  = B8.unpack (idBytes (Set.findMin a'))

  -- if all else fails, do nothing
  | otherwise = cur
  where
  pat' = mkId (B8.pack pat)
  cur' = mkId (B8.pack cur)

  (a,b)  = Set.split cur' users
  (_,a') = Set.split pat' a

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
  myNick = view (clientConnection . connNick) st

  alreadyOp =
    elemOf ( clientConnection
           . connChannels . ix chan
           . chanUsers . ix myNick
           . folded)
           'o'
           st

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
  do clientSend (modeCmd chan ["+b",idDenote nick <> "!*@*"]) st
     clientSend (kickCmd chan nick (Text.encodeUtf8 reason)) st
     return st
