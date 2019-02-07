{-# Language OverloadedStrings #-}
{-|
Module      : Client.Hook.Snotice
Description : Hook for sorting some service notices into separate windows.
Copyright   : (c) Eric Mertens 2019
License     : ISC
Maintainer  : emertens@gmail.com

These sorting rules are based on the kinds of messages that freenode's
ircd-seven sends.

-}
module Client.Hook.Snotice
  ( snoticeHook
  ) where

import qualified Data.Text as Text

import           Client.Hook
import           Irc.Message
import           Irc.UserInfo

snoticeHook :: MessageHook
snoticeHook = MessageHook "snotice" True remap

remap ::
  IrcMsg -> MessageResult

remap (Notice (UserInfo n@"services." "" "") _ msg) =
  RemapMessage (Notice (UserInfo n "" "*") "{services}" msg)

remap (Notice (UserInfo u "" "") _ msg)
  | "*** Notice -- K/DLINE active" `Text.isPrefixOf` msg ||
    "*** Notice -- Propagated ban" `Text.isPrefixOf` msg ||
    "*** Notice -- " `Text.isPrefixOf` msg && "min. K-Line for" `Text.isInfixOf` msg
  = RemapMessage (Notice (UserInfo u "" "*") "{kline}" msg)

  | "*** Notice -- OPERSPY " `Text.isPrefixOf` msg
  = RemapMessage (Notice (UserInfo u "" "*") "{operspy}" msg)

  | " is a possible spambot" `Text.isSuffixOf` msg
  = RemapMessage (Notice (UserInfo u "" "*") "{spambot}" msg)

  | "*** Notice -- Possible Flooder " `Text.isPrefixOf` msg
  = RemapMessage (Notice (UserInfo u "" "*") "{flood}" msg)

remap _ = PassMessage
