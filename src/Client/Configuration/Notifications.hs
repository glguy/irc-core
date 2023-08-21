{-# Language OverloadedStrings #-}
{-|
Module      : Client.Configuration.Notifications
Description : Options for running commands to notify users
Copyright   : (c) TheDaemoness, 2023
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Client.Configuration.Notifications ( NotifyWith(..), notifyCmd, notifyWithDefault ) where

import qualified Data.Text.Lazy as LText
import           System.Process.Typed (ProcessConfig, proc, setEnv)
import           System.Info (os)

data NotifyWith
  = NotifyWithCustom [String]
  | NotifyWithNotifySend
  | NotifyWithOsaScript
  | NotifyWithTerminalNotifier
  deriving Show

notifyCmd :: NotifyWith -> Maybe ((LText.Text, LText.Text) -> ProcessConfig () () ())
notifyCmd (NotifyWithCustom (cmd:args)) = Just $ \(header, body) ->
  proc cmd (args ++ [LText.unpack header, LText.unpack body])
notifyCmd NotifyWithNotifySend = Just $ \(header, body) ->
  proc "notify-send" ["-a", "glirc", LText.unpack header, LText.unpack body]
notifyCmd NotifyWithOsaScript = Just $ \(header, body) ->
  setEnv [("_GLIRC_NOTIF_HEADER", LText.unpack header), ("_GLIRC_NOTIF_BODY", LText.unpack body)] $
  proc "osascript" ["-e", script]
  where
    script = "display notification system attribute \"_GLIRC_NOTIF_BODY\" with title \"glirc\" with subtitle system attribute \"_GLIRC_NOTIF_HEADER\""
notifyCmd NotifyWithTerminalNotifier = Just $ \(header, body) ->
  proc "terminal-notifier" ["-title", "glirc", "-subtitle", LText.unpack header, "-message", LText.unpack body]
notifyCmd _ = Nothing

notifyWithDefault :: NotifyWith
notifyWithDefault = case os of
  "darwin" -> NotifyWithOsaScript
  "linux"  -> NotifyWithNotifySend
  _        -> NotifyWithCustom []
