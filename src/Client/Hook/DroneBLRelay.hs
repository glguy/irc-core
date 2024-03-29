{-# Language QuasiQuotes, OverloadedStrings #-}
{-|
Module      : Client.Hook.DroneBLRelay
Description : Hook for interpreting DroneBL relay messages
Copyright   : (c) Eric Mertens 2019
License     : ISC
Maintainer  : emertens@gmail.com

The #dronebl channels use a bot to relay messages from
other networks. This hook integrates those messages into the native
format.

-}
module Client.Hook.DroneBLRelay
  ( droneblRelayHook
  ) where

import Client.Hook (MessageHook(..), MessageResult(..))
import Data.Foldable (asum)
import Data.List (uncons)
import Data.Text (Text)
import Data.Text qualified as Text
import Irc.Identifier (mkId, Identifier)
import Irc.Message (IrcMsg(Mode, Privmsg, Ctcp, Join, Part, Quit, Nick, Kick), Source(Source))
import Irc.UserInfo (UserInfo(..))
import StrQuote (str)
import Text.Regex.TDFA (match, defaultCompOpt, defaultExecOpt, Regex)
import Text.Regex.TDFA.String (compile)

-- | Hook for mapping messages in #dronebl
-- to appear like native messages.
droneblRelayHook :: [Text] -> Maybe MessageHook
droneblRelayHook args = Just (MessageHook "droneblrelay" False (remap (map mkId args)))

-- | Remap messages from #dronebl that match one of the
-- rewrite rules.
remap :: [Identifier] -> IrcMsg -> MessageResult
remap nicks (Privmsg (Source (UserInfo nick _ _) _) chan@"#dronebl" msg)
  | nick `elem` nicks
  , Just sub <- rules chan msg = RemapMessage sub
remap _ _ = PassMessage

-- | Generate a replacement message for a chat message
-- when the message matches one of the replacement rules.
rules ::
  Identifier {- ^ channel -} ->
  Text       {- ^ message -} ->
  Maybe IrcMsg
rules chan msg =
  asum
    [ rule (chatMsg chan) chatRe msg
    , rule (actionMsg chan) actionRe msg
    , rule (joinMsg chan) joinRe msg
    , rule (partMsg chan) partRe msg
    , rule quitMsg quitRe msg
    , rule nickMsg nickRe msg
    , rule (kickMsg chan) kickRe msg
    , rule (modeMsg chan) modeRe msg
    ]

-- | Match the message against the regular expression and use the given
-- consume to consume all of the captured groups.
rule ::
  Rule r =>
  r     {- ^ capture consumer   -} ->
  Regex {- ^ regular expression -} ->
  Text  {- ^ message            -} ->
  Maybe IrcMsg
rule mk re s =
  case map (map Text.pack) (match re (Text.unpack s)) of
    [_:xs] -> matchRule xs mk
    _      -> Nothing

chatRe, actionRe, joinRe, quitRe, nickRe, partRe, kickRe, modeRe :: Regex
Right chatRe   = compRe [str|^<([^>]+)> (.*)$|]
Right actionRe = compRe [str|^\* ([^ ]+) (.*)$|]
Right joinRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) \(([^@]+)@([^)]+)\) has joined the channel$|]
Right quitRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) has signed off \((.*)\)$|]
Right nickRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) changed nick to ([^ ]+)$|]
Right partRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) has left the channel( \((.*)\))?$|]
Right kickRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) has been kicked by ([^ ]+) \((.*)\)$|]
Right modeRe   = compRe [str|^\*\*\* \[([^]]+)\] ([^ ]+) sets mode (.*)$|]

-- | Compile a regular expression for using in message matching.
compRe ::
  Text                {- ^ regular expression           -} ->
  Either String Regex {- ^ error or compiled expression -}
compRe = compile defaultCompOpt defaultExecOpt . Text.unpack

------------------------------------------------------------------------

chatMsg ::
  Identifier {- ^ channel  -} ->
  Text       {- ^ nickname -} ->
  Text       {- ^ message  -} ->
  IrcMsg
chatMsg chan nick msg =
  Privmsg
    (userInfo nick)
    chan
    msg

actionMsg ::
  Identifier {- ^ channel  -} ->
  Text       {- ^ nickname -} ->
  Text       {- ^ message  -} ->
  IrcMsg
actionMsg chan nick msg =
  Ctcp
    (userInfo nick)
    chan
    "ACTION"
    msg

joinMsg ::
  Identifier {- ^ channel  -} ->
  Text       {- ^ server   -} ->
  Text       {- ^ nickname -} ->
  Text       {- ^ username -} ->
  Text       {- ^ hostname -} ->
  IrcMsg
joinMsg chan srv nick user host =
  Join
    (Source (UserInfo (mkId (nick <> "@" <> srv)) user host) "")
    chan
    "" -- account
    "" -- gecos

partMsg ::
  Identifier {- ^ channel        -} ->
  Text       {- ^ server         -} ->
  Text       {- ^ nickname       -} ->
  Text       {- ^ reason wrapper -} ->
  Text       {- ^ reason         -} ->
  IrcMsg
partMsg chan srv nick msg_outer msg =
  Part
    (userInfo (nick <> "@" <> srv))
    chan
    (if Text.null msg_outer then Nothing else Just msg)

quitMsg ::
  Text {- ^ server       -} ->
  Text {- ^ nickname     -} ->
  Text {- ^ quit message -} ->
  IrcMsg
quitMsg srv nick msg =
  Quit
    (userInfo (nick <> "@" <> srv))
    (Just msg)

nickMsg ::
  Text {- ^ server   -} ->
  Text {- ^ old nick -} ->
  Text {- ^ new nick -} ->
  IrcMsg
nickMsg srv old new =
  Nick
    (userInfo (old <> "@" <> srv))
    (mkId (new <> "@" <> srv))

kickMsg ::
  Identifier {- ^ channel     -} ->
  Text       {- ^ server      -} ->
  Text       {- ^ kickee nick -} ->
  Text       {- ^ kicker nick -} ->
  Text       {- ^ reason      -} ->
  IrcMsg
kickMsg chan srv kickee kicker reason =
  Kick
    (userInfo (kicker <> "@" <> srv))
    chan
    (mkId (kickee <> "@" <> srv))
    reason

modeMsg ::
  Identifier {- ^ channel     -} ->
  Text       {- ^ server      -} ->
  Text       {- ^ nickname    -} ->
  Text       {- ^ modes       -} ->
  IrcMsg
modeMsg chan srv nick modes =
  Mode
    (userInfo (nick <> "@" <> srv))
    chan
    (Text.words modes)

-- | Construct dummy user info when we don't know the user or host part.
userInfo ::
  Text {- ^ nickname -} ->
  Source
userInfo nick = Source (UserInfo (mkId nick) "*" "*") ""

------------------------------------------------------------------------

-- | The class allows n-ary functions of the form
-- `Text -> Text -> ... -> IrcMsg` to be used to exhaustively consume the
-- matched elements of a regular expression.
class Rule a where
  matchRule :: [Text] -> a -> Maybe IrcMsg

instance (RuleArg a, Rule b) => Rule (a -> b) where
  matchRule tts f =
    do (t,ts) <- uncons tts
       a      <- matchArg t
       matchRule ts (f a)

instance Rule IrcMsg where
  matchRule args ircMsg
    | null args = Just ircMsg
    | otherwise = Nothing

class    RuleArg a    where matchArg :: Text -> Maybe a
instance RuleArg Text where matchArg = Just
