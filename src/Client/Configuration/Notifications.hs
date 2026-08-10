{-# Language OverloadedStrings #-}
{-|
Module      : Client.Configuration.Notifications
Description : Options for running commands to notify users
Copyright   : (c) TheDaemoness, 2023
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Client.Configuration.Notifications ( NotifyWith(..), NotifyWhile(..), notifySpec, notifyWhileSpec ) where

import           Config.Schema (ValueSpec, atomSpec, nonemptySpec, stringSpec, (<!>))
import qualified Data.List.NonEmpty as NonEmpty

data NotifyWith
  = NotifyWithCustom [String]
  | NotifyWithDefault
  | NotifyWithNotifySend
  | NotifyWithOsaScript
  | NotifyWithTerminalNotifier
  | NotifyWithTerminal
  deriving Show

data NotifyWhile
  = NotifyWhileUnfocused
  | NotifyWhileFocused
  | NotifyWhileAlways
  deriving Show

notifySpec :: ValueSpec NotifyWith
notifySpec =
  NotifyWithCustom []        <$ atomSpec "no"  <!>
  NotifyWithDefault          <$ atomSpec "yes" <!>
  NotifyWithNotifySend       <$ atomSpec "notify-send" <!>
  NotifyWithOsaScript        <$ atomSpec "osascript" <!>
  NotifyWithTerminalNotifier <$ atomSpec "terminal-notifier" <!>
  NotifyWithTerminal         <$ atomSpec "terminal" <!>
  NotifyWithCustom . NonEmpty.toList <$> nonemptySpec stringSpec

notifyWhileSpec :: ValueSpec NotifyWhile
notifyWhileSpec =
  NotifyWhileUnfocused <$ atomSpec "unfocused" <!>
  NotifyWhileFocused <$ atomSpec "focused" <!>
  NotifyWhileAlways <$ atomSpec "always"
