{-# Language QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Client.Hook.FreRelay
Description : Hook for interpreting FreFrelay messages
Copyright   : (c) Eric Mertens 2019
License     : ISC
Maintainer  : emertens@gmail.com

The #dronebl channel uses the FreRelay bot to relay messages from
other networks. This hook integrates those messages into the native
format.

-}
module Client.Hook.FreRelay
  ( freRelayHook
  ) where

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Foldable (asum)

import           Client.Hook
import           Irc.Message
import           Irc.Identifier (mkId, Identifier)
import           Irc.UserInfo

import           Text.Regex.TDFA
import           Text.Regex.TDFA.String
import           StrQuote (str)

freRelayHook :: MessageHook
freRelayHook = MessageHook "frerelay" False remap

remap :: IrcMsg -> MessageResult
remap (Privmsg src chan msg)
  | userNick src == "frerelay"
  , chan == "#dronebl"
  , Just sub <- rules chan msg
  = RemapMessage sub

remap _ = PassMessage

rules :: Identifier -> Text -> Maybe IrcMsg
rules chan msg =
  asum
    [ rule (chatMsg chan) chatRe s
    , rule (actionMsg chan) actionRe s
    , rule (joinMsg chan) joinRe s
    , rule (partMsg chan) partRe s
    , rule quitMsg quitRe s
    , rule nickMsg nickRe s
    ]
  where
    s = Text.unpack msg

rule :: ([String] -> IrcMsg) -> Regex -> String -> Maybe IrcMsg
rule mk re s =
  case match re s of
    [_:xs] -> Just $! mk xs
    _      -> Nothing

chatRe, actionRe, joinRe, quitRe, nickRe, partRe :: Regex
Right chatRe   = compRe [str|^<([^>]+)> (.*)$|]
Right actionRe = compRe [str|^\* ([^ ]+) (.*)$|]
Right joinRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) \(([^@]+)@([^)]+)\) has joined the channel$|]
Right quitRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) has signed off \((.*)\)$|]
Right nickRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) changed nick to ([^ ]+)$|]
Right partRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) has left the channel$|]

compRe :: String -> Either String Regex
compRe = compile defaultCompOpt defaultExecOpt

chatMsg :: Identifier -> [String] -> IrcMsg
chatMsg chan [nick, msg] = Privmsg (UserInfo (mkId (Text.pack nick)) "*" "*") chan (Text.pack msg)

actionMsg :: Identifier -> [String] -> IrcMsg
actionMsg chan [nick, msg] = Ctcp (UserInfo (mkId (Text.pack nick)) "*" "*") chan "ACTION" (Text.pack msg)

joinMsg :: Identifier -> [String] -> IrcMsg
joinMsg chan [srv, nick, user, host] =
  Join (UserInfo (mkId (Text.pack (nick ++ "@" ++ srv))) (Text.pack user) (Text.pack host))
       chan
       "" -- account

partMsg :: Identifier -> [String] -> IrcMsg
partMsg chan [srv, nick] =
  Part (UserInfo (mkId (Text.pack (nick ++ "@" ++ srv))) "*" "*")
       chan
       Nothing

quitMsg :: [String] -> IrcMsg
quitMsg [srv, nick, msg] =
  Quit (UserInfo (mkId (Text.pack (nick ++ "@" ++ srv))) "*" "*")
       (Just (Text.pack msg))

nickMsg :: [String] -> IrcMsg
nickMsg [srv, old, new] =
  Nick (UserInfo (mkId (Text.pack (old ++ "@" ++ srv))) "*" "*")
       (mkId (Text.pack (new ++ "@" ++ srv)))
