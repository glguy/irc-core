{-# Language QuasiQuotes, OverloadedStrings #-}
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
import           Data.Text (Text)
import           Data.List (find)
import           Text.Regex.TDFA
import           Text.Regex.TDFA.String

import           Client.Hook
import           Irc.Message
import           Irc.Identifier (mkId, Identifier)
import           Irc.UserInfo
import           Language.Haskell.TH
import           StrQuote (str)

snoticeHook :: MessageHook
snoticeHook = MessageHook "snotice" True remap

remap ::
  IrcMsg -> MessageResult

remap (Notice (UserInfo u "" "") _ msg)
  | Just msg1 <- Text.stripPrefix "*** Notice -- " msg
  , let msg2 = Text.filter (\x -> x /= '\x02' && x /= '\x0f') msg1
  , Just (_lvl, cat) <- characterize msg2
  = RemapMessage (Notice (UserInfo u "" "*") cat msg1)

remap _ = PassMessage

toPattern :: (Int, String, String) -> (Int, Identifier, Regex)
toPattern (lvl, cat, reStr) =
  case compile co eo reStr of
    Left e  -> error e
    Right r -> (lvl, mkId (Text.pack ('~':cat)), r)
  where
    co = CompOption
      { caseSensitive  = True
      , multiline      = False
      , rightAssoc     = True
      , newSyntax      = True
      , lastStarGreedy = True }
    eo = ExecOption
      { captureGroups  = False }

characterize :: Text -> Maybe (Int, Identifier)
characterize txt =
  do let s = Text.unpack txt
     (lvl, cat, _) <- find (\(_, _, re) -> matchTest re s) patterns
     pure (lvl, cat)

patterns :: [(Int, Identifier, Regex)]
patterns = map toPattern
    []
