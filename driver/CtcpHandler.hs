{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module CtcpHandler where

import Control.Lens
import Control.Monad
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Maybe
import Data.Time
import Data.Version (showVersion)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import ClientState
import DCC
import ServerSettings
import Irc.Format
import Irc.Message
import Irc.Cmd
import Paths_irc_core (version)

versionString :: ByteString
versionString = "glirc " <> B8.pack (showVersion version)

sourceString :: ByteString
sourceString = "https://github.com/glguy/irc-core"

ctcpHandler :: EventHandler
ctcpHandler = EventHandler
  { _evName = "CTCP replies"
  , _evOnEvent = \_ msg st ->

       do let sender = views mesgSender userNick msg
          forOf_ (mesgType . _CtcpReqMsgType) msg $ \(command,params) ->

               -- Don't send responses to ignored users
               unless (view (clientIgnores . contains sender) st) $
                 case command of
                   "CLIENTINFO" ->
                     clientSend (ctcpResponseCmd sender "CLIENTINFO"
                                   "ACTION CLIENTINFO FINGER PING SOURCE TIME USERINFO VERSION") st
                   "VERSION" ->
                     clientSend (ctcpResponseCmd sender "VERSION" versionString) st
                   "USERINFO" ->
                     clientSend (ctcpResponseCmd sender "USERINFO"
                                    (views (clientServer0 . ccServerSettings . ssUserInfo)
                                           (Text.encodeUtf8 . Text.pack) st)) st
                   "PING" ->
                     clientSend (ctcpResponseCmd sender "PING" params) st
                   "SOURCE" ->
                     clientSend (ctcpResponseCmd sender "SOURCE" sourceString) st
                   "FINGER" ->
                     clientSend (ctcpResponseCmd sender "FINGER"
                                    "Username and idle time unavailable") st
                   "TIME" -> do
                     now <- getZonedTime
                     let resp = formatTime defaultTimeLocale "%a %d %b %Y %T %Z" now
                     clientSend (ctcpResponseCmd sender "TIME" (B8.pack resp)) st
                   _ -> return ()

          -- reschedule handler
          return (over clientAutomation (cons ctcpHandler) st)
  }

dccHandler :: FilePath -> EventHandler
dccHandler outDir = EventHandler
  { _evName = "DCC handler"
  , _evOnEvent = \ident msg st ->
         return $ over clientAutomation (cons (dccHandler outDir))
                       (queueOffer outDir ident msg st)
  }

queueOffer :: FilePath -> Identifier -> IrcMessage
           -> ClientState -> ClientState
queueOffer outDir _ msg st =
  fromMaybe st $
    notIgnored >> isCtcpMsg >>= isDCCcommand
    >>= pure . userConfirm sender . storeOffer
  where
    space = 0x20
    sender = views mesgSender userNick msg
    ctime  = view mesgStamp msg

    -- Could be an 'if then else' but the shortcircuit of Maybe is
    -- clearer. () really could be any type.
    notIgnored :: Maybe ()
    notIgnored = if not (view (clientIgnores . contains sender) st)
                    then Just () else Nothing

    isCtcpMsg :: Maybe (ByteString, ByteString)
    isCtcpMsg = preview (mesgType . _CtcpReqMsgType) msg

    isDCCcommand :: (ByteString, ByteString) -> Maybe (FourTuple ByteString)
    isDCCcommand ("DCC", params)
      | [type',  bName, bAddr, bPort, bSize] <- take 5 (B.split space params)
      , type' == "SEND" = Just (bName, bAddr, bPort, bSize)
    isDCCcommand _ = Nothing

    -- Store the offer until the user accepts it on /dcc accept
    storeOffer :: FourTuple ByteString -> ClientState
    storeOffer offer =
      set (clientServer0 . ccHoldDccTrans . _1 . at sender)
          (Just (parseDccOffer ctime outDir offer)) st

-- todo(slack): better message to the user (this is a hack)
-- | We assume ctcpHandler already ran and created the corresponding window.
userConfirm :: Identifier -> ClientState -> ClientState
userConfirm sender st =
  let questionText =
        "You have a pending DCC transfer from this sender. Issue /dcc "
        <> "accept or /dcc cancel. If started, you can issue /dcc cancel "
        <> "to stop the transfer from this window"
  in addMessage sender (set mesgType (PrivMsgType questionText) defaultIrcMessage) st
