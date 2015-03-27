{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module CtcpHandler (ctcpHandler) where

import Control.Lens
import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Time
import Data.Version (showVersion)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import ClientState
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
