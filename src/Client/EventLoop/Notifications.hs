{-# Language OverloadedStrings #-}

{-|
Module      : Client.EventLoop.Notification
Description : Notification support
Copyright   : (c) TheDaemoness, 2025
License     : ISC
Maintainer  : emertens@gmail.com

This module dispatches notifications,
which are status updates that are shown outside of the TUI.
-}

module Client.EventLoop.Notifications ( Notification, doNotify ) where

import           Client.Configuration.Notifications ( NotifyWith(..) )
import           Control.Exception (SomeException, catch)
import           Control.Monad (void)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy.IO as LTextIO
import           Data.Text.Lazy
import           System.Environment (lookupEnv)
import           System.Process.Typed (ProcessConfig, proc, startProcess, setEnv, setStdin, setStdout, setStderr, nullStream)
import           System.Info (os)
import           System.IO (hFlush, stdout)

type Notification = (Text, Text);

osDefault :: NotifyWith
osDefault = case os of
  "darwin" -> NotifyWithOsaScript
  "linux"  -> NotifyWithNotifySend
  _        -> NotifyWithCustom []

putStrAndFlush :: Text -> IO ()
putStrAndFlush txt = LTextIO.putStr txt >> hFlush stdout

doNotify :: NotifyWith -> Notification -> IO ()
doNotify (NotifyWithCustom []) _ = return ()
doNotify NotifyWithDefault notif = do
  renderer <- renderOsc
  case renderer of
    Just renderer' -> putStrAndFlush $ renderer' notif
    Nothing -> doNotify osDefault notif
doNotify NotifyWithTerminal notif = do
  renderer <- renderOsc
  putStrAndFlush $ (fromMaybe renderOsc777 renderer) notif
doNotify (NotifyWithCustom (cmd:args)) (header, body) = spawnNotifier
    $ proc cmd (args ++ [unpack header, unpack body])
doNotify NotifyWithNotifySend (header, body) = spawnNotifier
    $ proc "notify-send" ["-a", "glirc", unpack header, unpack body]
doNotify NotifyWithOsaScript (header, body) = spawnNotifier
    $ setEnv [("_GLIRC_NOTIF_HEADER", unpack header), ("_GLIRC_NOTIF_BODY", unpack body)]
    $ proc "osascript" ["-e", script]
    where
      script = "display notification (system attribute \"_GLIRC_NOTIF_BODY\") with title \"glirc\" subtitle (system attribute \"_GLIRC_NOTIF_HEADER\")"
doNotify NotifyWithTerminalNotifier (header, body) = spawnNotifier
    $ proc "terminal-notifier" ["-title", "glirc", "-subtitle", unpack header, "-message", "\\" <> unpack body]

spawnNotifier :: ProcessConfig i o e -> IO ()
spawnNotifier cmd = do
  let procCfg = setStdin nullStream . setStdout nullStream $ setStderr nullStream cmd
  catch (void (startProcess procCfg)) handleException
  where
    -- TODO: May be a nicer way to handle notification failure than just silently squashing the exception
    handleException :: SomeException -> IO ()
    handleException _ = return ()

-- Here be TUI dragons.
-- There are three different noteworthy OSC sequences for telling terminal emulators to display a notification.
-- By far the most-widely supported is OSC 777 notify.
-- However, we also need to support OSC 9 on iTerm2 (and ONLY iTerm2) and OSC 99 on kitty.
-- Technically other terminals that support OSC 99 (if any exist, not sure) can be queried for support.
-- We're not doing that. That'll be the responsibility of vty if it ever gets that functionality.

type RenderFn = Notification -> Text

makeOsc :: Text -> Text -> Text
makeOsc code payload = mconcat ["\ESC]", code, ";", payload, "\ESC\\"]

renderOsc777 :: RenderFn
renderOsc777 (header, body) = makeOsc "777;notify" $ mconcat [header, ";", body]

renderOsc :: IO (Maybe RenderFn)
renderOsc = do
  term <- lookupEnv "TERM"
  case tryModify (stripPrefix "xterm-") . tryModify (stripSuffix "-direct") . pack <$> term of
    -- Special terminals
    Just "iterm2"  -> return $ Just $ \(header, body) ->
      makeOsc "9" $ mconcat [header, ": ", body]
    Just "kitty"  -> return $ Just $ \(header, body) -> mconcat
      [ (makeOsc "99;i=1:d=0"    header)
      , (makeOsc "99;i=1:p=body" body)
      ]
    -- Everything else
    Just "foot"         -> return $ Just renderOsc777
    Just "ghostty"      -> return $ Just renderOsc777
    Just "rxvt-unicode" -> return $ Just renderOsc777
    Just "wezterm"      -> return $ Just renderOsc777
    _ -> return Nothing
  where
    tryModify :: (Text -> Maybe Text) -> Text -> Text
    tryModify f str = fromMaybe str $ f str
