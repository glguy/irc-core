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
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Char
import Data.Foldable (traverse_)
import Data.Functor
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Graphics.Vty
import Network
import Network.IRC.ByteString.Parser
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
import Irc.Model

import CommandArgs
import ClientState
import Views.Channel
import Views.ChannelInfo
import Views.BanList
import Views.User
import Views.Server
import qualified EditBox as Edit

main :: IO ()
main = do
  args <- getCommandArgs

  h    <- connectTo (view cmdArgServer args) (PortNumber (fromIntegral (view cmdArgPort args)))
  hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
  hSetEncoding h utf8

  hErr <- openFile "debug.txt" WriteMode
  hSetBuffering hErr NoBuffering

  B.hPut h capLsCmd
  B.hPut h (capReqCmd "away-notify account-notify extended-join")
  B.hPut h capEndCmd
  traverse_ (B.hPut h . passCmd . B8.pack) (view cmdArgPassword args)
  B.hPut h (nickCmd (B8.pack (view cmdArgNick args)))
  B.hPut h (userCmd (B8.pack (view cmdArgUser args))
                     "0"
                     (B8.pack (view cmdArgReal args)))

  vtyEventChan <- atomically newTChan
  socketChan   <- atomically newTChan

  let conn0 = defaultIrcConnection { _connNick = B8.pack (view cmdArgNick args) }

  cfg <- standardIOConfig
  bracket (mkVty cfg) shutdown $ \vty ->
    do _ <- forkIO (socketLoop socketChan h hErr)
       _ <- forkIO (vtyEventLoop vtyEventChan vty)
       (width,height) <- displayBounds (outputIface vty)
       driver vty vtyEventChan socketChan ClientState
         { _clientSocket          = h
         , _clientErrors          = hErr
         , _clientConnection      = conn0
         , _clientFocus           = ServerFocus
         , _clientDetailView      = False
         , _clientEditBox         = Edit.empty
         , _clientTabPattern      = Nothing
         , _clientInputHistory    = []
         , _clientInputHistoryPos = 0
         , _clientScrollPos       = 0
         , _clientHeight          = height
         , _clientWidth           = width
         , _clientMessagesSeen    = mempty
         }

driver :: Vty -> TChan Event -> TChan MsgFromServer -> ClientState -> IO ()
driver vty vtyEventChan ircMsgChan st =
  do update vty (picForState st)
     e <- readEitherTChan vtyEventChan ircMsgChan
     case e of
       Left vtyEvent -> processVtyEvent vtyEvent
       Right msg     -> processIrcMsg   msg

  where
  continue = driver vty vtyEventChan ircMsgChan
           . updateNewMessages

  processVtyEvent event =
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

  processIrcMsg msg =
    do now <- getCurrentTime
       r <- runLogic (interpretLogicOp ircMsgChan)
                     (fmap (return . Right)
                           (advanceModel now msg (view clientConnection st)))
       case r of
         Left e ->
           do hPutStrLn (view clientErrors st) ("!!! " ++ e)
              continue st
         Right conn' ->
           continue (set clientConnection conn' st)

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
keyEvent KLeft       _       st = return $ clearTabPattern $ over clientEditBox Edit.left st
keyEvent KRight      _       st = return $ clearTabPattern $ over clientEditBox Edit.right st
keyEvent KHome       _       st = return $ clearTabPattern $ over clientEditBox Edit.home st
keyEvent KEnd        _       st = return $ clearTabPattern $ over clientEditBox Edit.end st
keyEvent (KChar 'a') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.home st
keyEvent (KChar 'e') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.end st
keyEvent (KChar 'u') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.killHome st
keyEvent (KChar 'k') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.killEnd st
keyEvent (KChar 'w') [MCtrl] st = return $ clearTabPattern $ over clientEditBox Edit.killWord st
keyEvent (KChar '\t') []     st = return $ tabComplete st
keyEvent (KChar c)   []      st = return $ clearTabPattern $ over clientEditBox (Edit.insert c) st
keyEvent KEnter      []      st = case clientInput st of
                                    []          -> return st
                                    '/':command -> commandEvent command st
                                    _           -> doSendMessageCurrent st
keyEvent _           _       st = return st


-- TODO: Don't scroll off the end of the channel

scrollOffset :: ClientState -> Int
scrollOffset st = max 1 (view clientHeight st - 4)

scrollUp :: ClientState -> ClientState
scrollUp st = over clientScrollPos (+ scrollOffset st) st

scrollDown :: ClientState -> ClientState
scrollDown st = over clientScrollPos (\x -> max 0 (x - scrollOffset st)) st

doSendMessageCurrent :: ClientState -> IO ClientState
doSendMessageCurrent st =
  case view clientFocus st of
    ChannelFocus c -> doSendMessage c (Text.pack (clientInput st)) (clearInput st)
    UserFocus u    -> doSendMessage u (Text.pack (clientInput st)) (clearInput st)
    _ -> return st

doSendMessage :: ByteString -> Text -> ClientState -> IO ClientState
doSendMessage target message st =
  do send'
     now <- getCurrentTime
     let addMessageToConnection =
             over clientConnection (recordMessage (fakeMsg now) target)
     return (addMessageToConnection st)

  where
  fakeMsg now = IrcMessage
    { _mesgSender = Left who
    , _mesgContent = message
    , _mesgType = PrivMsgType
    , _mesgStamp = now
    , _mesgModes = ""
    , _mesgMe = True
    }

  who = UserInfo (view (clientConnection . connNick) st)
                 Nothing
                 Nothing

  send' = B.hPut (view clientSocket st)
                 (privMsgCmd target (Text.encodeUtf8 message))


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
      return (set clientFocus ServerFocus st')

    "query"  :- user :- "" ->
      return (set clientFocus (UserFocus        (B8.pack user)) st')

    "channel" :- chan :- "" ->
      return (set clientFocus (ChannelFocus     (B8.pack chan)) st')

    "channelinfo" :- chan :- "" ->
      return (set clientFocus (ChannelInfoFocus (B8.pack chan)) st')

    "bans" :- chan :- "" ->
      return (set clientFocus (BanListFocus (B8.pack chan)) st')

    -- chat
    "me" :- msg -> doActionMsg msg st'
    "msg" :- target :- msg -> doSendMessage (B8.pack target) (Text.pack msg) st'

    -- raw
    "quote" :- rest ->
      doQuote rest st'

    -- channel commands
    "join" :- c :- "" ->
      st' <$ B.hPut (view clientSocket st) (joinCmd (toB c) Nothing)

    "join" :- c :- k :- "" ->
      st' <$ B.hPut (view clientSocket st) (joinCmd (toB c) (Just (toB k)))

    _                    -> return st

  where
  st' = clearInput st
  toB = Text.encodeUtf8 . Text.pack

doQuote :: String -> ClientState -> IO ClientState
doQuote cmd st = st <$ hPutStrLn (view clientSocket st) cmd

-- TODO : Add fake Action message to connection
doActionMsg :: String -> ClientState -> IO ClientState
doActionMsg msg st =
  case view clientFocus st of
    ChannelFocus chan -> send chan >> return st
    UserFocus user -> send user >> return st
    _ -> return st
  where
  send target = B.hPut
                  (view clientSocket st)
                  (actionCmd target (Text.encodeUtf8 (Text.pack msg)))

------------------------------------------------------------------------
-- Primary UI rendering
------------------------------------------------------------------------

picForState :: ClientState -> Picture
picForState st = Picture
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

  where
  everythingBeforeInput = vertCat
    [ titlebar
    , string defAttr (replicate (view clientWidth st) '─')
    , messageFrame
    ]
  divider = dividerImage st

  messageFrame =
    case view clientFocus st of
      ChannelFocus{}
        | view clientDetailView st -> detailedImageForState st
        | otherwise                -> compressedImageForState st
      UserFocus u                  -> queryImage st u
      BanListFocus chan -> banListImage chan st
      ChannelInfoFocus chan -> channelInfoImage chan st
      ServerFocus           -> serverInfoImage st

  titlebar =
    case view clientFocus st of
      ServerFocus    -> string defAttr "Server"
      ChannelFocus c -> utf8Bytestring' defAttr c <|> topicbar c
      UserFocus    u -> utf8Bytestring' defAttr u
      BanListFocus c -> string defAttr "Bans: "
                    <|> utf8Bytestring' defAttr c
      ChannelInfoFocus c -> string defAttr "Channel Info: "
                        <|> utf8Bytestring' defAttr c

  topicbar chan =
    case preview (clientConnection . connChannelIx chan . chanTopic . folded . _1) st of
      Just topic -> string defAttr " - " <|> text' (withForeColor defAttr green) topic
      Nothing    -> emptyImage

textbox :: String -> Int -> Int -> Image
textbox str pos width
  | pos > width - 2     = string defAttr (drop (pos - width + 1) str)
                      <|> ending

  | len <= width - 2    = beginning
                      <|> string defAttr str
                      <|> ending

  | otherwise           = beginning
                      <|> string defAttr (take (width-1) str)
  where
  len = length str

  beginning = char (withForeColor defAttr brightBlack) '^'
  ending    = char (withForeColor defAttr brightBlack) '$'

dividerImage :: ClientState -> Image
dividerImage st
  = extendToWidth
  $ ifoldr (\i x xs -> drawOne i x <|> xs) emptyImage
  $ countNewMessages st

  where
  drawOne :: CI ByteString -> Int -> Image
  drawOne i n
    | Just i == active
          = string defAttr "─["
        <|> coloredInt n
        <|> string defAttr "]"

    | otherwise =
            string defAttr "─<"
        <|> coloredInt n
        <|> string defAttr ">"

  coloredInt :: Int -> Image
  coloredInt x
    | x > 0     = string (withForeColor defAttr green) (show x)
    | otherwise = string (withForeColor defAttr brightBlack) (show x)

  extendToWidth img =
    img <|> string defAttr (replicate (view clientWidth st - imageWidth img) '─')

  active =
    case view clientFocus st of
      ChannelFocus c -> Just (CI.mk c)
      UserFocus    u -> Just (CI.mk u)
      _              -> Nothing


------------------------------------------------------------------------
-- Event loops
------------------------------------------------------------------------

socketLoop :: TChan MsgFromServer -> Handle -> Handle -> IO a
socketLoop chan h hErr = forever (atomically . writeTChan chan =<< getOne h hErr)

vtyEventLoop :: TChan Event -> Vty -> IO a
vtyEventLoop chan vty = forever (atomically . writeTChan chan =<< nextEvent vty)

interpretLogicOp ::
  TChan MsgFromServer {- ^ input -} ->
  LogicOp (IO (Either String a)) ->
  IO (Either String a)
interpretLogicOp ircChan (Expect k) = k =<< atomically (readTChan ircChan)
interpretLogicOp _ (Failure e) = return (Left e)

getOne :: Handle -> Handle -> IO MsgFromServer
getOne h hErr =
    do xs <- ircGetLine h
       case parseIrcMsg xs of
         Left e -> hPrint hErr (e, xs) >> getOne h hErr
         Right msg ->
           case ircMsgToServerMsg (copyIRCMsg msg) of
             Just (Ping x) -> B.hPut h (pongCmd x) >> getOne h hErr
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
          let users = Map.keysSet (view (clientConnection . connChannelIx c . chanUsers) st)
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

tabSearch :: String -> String -> Set (CI ByteString) -> String
tabSearch pat cur users
  | not (Set.null b)
  , B.isPrefixOf (CI.foldedCase pat') (CI.foldedCase (Set.findMin b))
  = B8.unpack (CI.original (Set.findMin b))

  -- wrap around when pat is a user
  | Set.member pat' users
  = pat -- TODO: Use original case

  -- wrap around when pat is not a user
  | not (Set.null a')
  = B8.unpack (CI.original (Set.findMin a'))

  -- if all else fails, do nothing
  | otherwise = cur
  where
  pat' = CI.mk (B8.pack pat)
  cur' = CI.mk (B8.pack cur)

  (a,b)  = Set.split cur' users
  (_,a') = Set.split pat' a
