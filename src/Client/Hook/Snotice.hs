{-# Language QuasiQuotes, OverloadedStrings #-}
{-|
Module      : Client.Hook.Snotice
Description : Hook for sorting some service notices into separate windows.
Copyright   : (c) Eric Mertens 2019
License     : ISC
Maintainer  : emertens@gmail.com

These sorting rules are based on the solanum server notices.

-}
module Client.Hook.Snotice
  ( snoticeHook
  ) where

import Client.Hook (MessageHook(MessageHook), MessageResult(..))
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Irc.Identifier (mkId, Identifier)
import Irc.Message (IrcMsg(Notice), Source(Source))
import Irc.UserInfo (UserInfo(UserInfo))
import StrQuote (str)
import Text.Regex.TDFA
import Text.Regex.TDFA.String (compile)

snoticeHook :: MessageHook
snoticeHook = MessageHook "snotice" True remap

remap ::
  IrcMsg -> MessageResult

remap (Notice (Source (UserInfo u "" "") _) _ msg)
  | Just msg1 <- Text.stripPrefix "*** Notice -- " msg
  , let msg2 = Text.filter (\x -> x /= '\x02' && x /= '\x0f') msg1
  , Just (lvl, cat) <- characterize msg2
  = if lvl < 1 then OmitMessage
               else RemapMessage (Notice (Source (UserInfo u "" "*") "") cat msg1)

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
    [
    -- PATTERN LIST, most common snotes
    -- Client connecting, more complete regex: ^Client connecting: [^ ]+ \([^ ]+@[^ ]+\) \[[^ ]+\] \{[^ ]+\} \[.*\]$
    (1, "c", [str|^Client connecting: |]),
    -- Client exiting, more complete regex: ^Client exiting: [^ ]+ \([^ ]+@[^ ]+\) \[.*\] \[[^ ]+\]$
    (0, "c", [str|^Client exiting: |]),
    -- Nick change
    (0, "c", [str|^Nick change: From |]),
    -- Connection limit, more complete regex: ^Too many user connections for [^ ]+![^ ]+@[^ ]+$
    (1, "u", [str|^Too many user connections for |]),
    -- Join alerts, more complete regex: ^User [^ ]+ \([^ ]+@[^ ]+\) trying to join #[^ ]* is a possible spambot$
    (1, "a", [str|^User [^ ]+ \([^ ]+\) trying to join #[^ ]* is a possible spambot|]),
    -- Kline hitting user
    (1, "k", [str|^K/DLINE active for|]),
    -- Connection limit, more complete regex: ^Too many local connections for [^ ]+![^ ]+@[^ ]+$
    (1, "u", [str|^Too many local connections for |]),
    -- Global kline added, more complete regex: ^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} added global [0-9]+ min. K-Line for \[[^ ]+\] \[.*\]$
    (2, "k", [str|^[^ ]+ added global [0-9]+ min. K-Line for |]),
    (2, "k", [str|^[^ ]+ added global [0-9]+ min. X-Line for |]),
    -- Global kline expiring, more complete regex: ^Propagated ban for \[[^ ]+\] expired$
    (0, "k", [str|^Propagated ban for |]),
    -- Chancreate
    (1, "u", [str|^[^ ]+ is creating new channel #|]),
    -- m_filter
    (0, "u", [str|^FILTER: |]),
    (0, "m", [str|^New filters loaded.$|]),
    (0, "m", [str|^Filtering enabled.$|]),
    -- Failed login
    (0, "f", [str|^Warning: [0-9]+ failed login attempts|]),
    -- Temporary kline added, more complete regex: ^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} added temporary [0-9]+ min. K-Line for \[[^ ]+\] \[.*\]$
    (1, "k", [str|^OperServ![^ ]+\{services\.\} added temporary [0-9]+ min. K-Line for \[[^ ]+\] \[Joining #|]), -- klinechans
    (1, "k", [str|^OperSyn![^ ]+\{syn\.\} added temporary [0-9]+ min. K-Line for \[[^ ]+\] \[You've been temporarily|]), -- lethals
    (2, "k", [str|^[^ ]+ added temporary [0-9]+ min. K-Line for |]),
    -- Nick collision
    (1, "m", [str|^Nick collision on|]),
    -- KILLs
    (0, "k", [str|^Received KILL message for [^ ]+\. From NickServ |]),
    (0, "k", [str|^Received KILL message for [^ ]+\. From syn Path: [^ ]+ \(Facility Blocked\)|]),
    (1, "k", [str|^Received KILL message for [^ ]+\. From syn Path: [^ ]+ \(Banned\)|]),
    (2, "k", [str|^Received KILL message|]),
    -- Teporary kline expiring, more complete regex: ^Temporary K-line for \[[^ ]+\] expired$
    (0, "k", [str|^Temporary K-line for |]),

    -- PATTERN LIST, uncommon snotes. regex performance isn't very important beyond this point
    (2, "a", [str|^Possible Flooder|]),
    (0, "a", [str|^New Max Local Clients: [0-9]+$|]),
    (1, "a", [str|^Excessive target change from|]),

    (1, "f", [str|^Failed (OPER|CHALLENGE) attempt - host mismatch|]),
    (3, "f", [str|^Failed (OPER|CHALLENGE) attempt|]), -- ORDER IMPORTANT - catch all failed attempts that aren't host mismatch

    (1, "k", [str|^Rejecting [XK]-Lined user|]),
    (1, "k", [str|^Disconnecting [XK]-Lined user|]),
    (1, "k", [str|^KLINE active for|]),
    (1, "k", [str|^XLINE active for|]),
    (3, "k", [str|^KLINE over-ruled for |]),
    (2, "k", [str|^[^ ]+ added global [0-9]+ min. K-Line from [^ ]+![^ ]+@[^ ]+\{[^ ]+\} for \[[^ ]+\] \[.*\]$|]),
    (2, "k", [str|^[^ ]+ added global [0-9]+ min. X-Line from [^ ]+![^ ]+@[^ ]+\{[^ ]+\} for \[[^ ]+\] \[.*\]$|]),
    (2, "k", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} has removed the global K-Line for: \[.*\]$|]),
    (2, "k", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} has removed the temporary K-Line for: \[.*\]$|]),
    (2, "k", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} added temporary [0-9]+ min. D-Line for \[[^ ]+\] \[.*\]$|]),
    (2, "k", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} has removed the X-Line for:|]),
    (2, "k", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is removing the X-Line for|]),
    (2, "k", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} has removed the temporary D-Line for:|]),

    (0, "m", [str|^Received SAVE message for|]),
    (0, "m", [str|^Ignored noop SAVE message for|]),
    (0, "m", [str|^Ignored SAVE message for|]),
    (0, "m", [str|^TS for #[^ ]+ changed from|]),
    (0, "m", [str|^Nick change collision from |]),
    (0, "m", [str|^Dropping IDENTIFIED|]),
    (1, "m", [str|^Got signal SIGHUP, reloading ircd conf\. file|]),
    (1, "m", [str|^Got SIGHUP; reloading|]),
    (1, "m", [str|^Updating database by request of system console\.$|]),
    (1, "m", [str|^Rehashing .* by request of system console\.$|]),
    (2, "m", [str|^Updating database by request of [^ ]+( \([^ ]+\))?\.$|]),
    (2, "m", [str|^Rehashing .* by request of [^ ]+( \([^ ]+\))?\.$|]),
    (2, "m", [str|.* is rehashing server config file$|]),
    (3, "m", [str|^".*", line [0-9+]|]), -- configuration syntax error!
    (0, "m", [str|^Ignoring attempt from [^ ]+( \([^ ]+\))? to set login name for|]),
    (1, "m", [str|^binding listener socket: 99 \(Cannot assign requested address\)$|]),
    (2, "m", [str|^binding listener socket: |]),

    (2, "o", [str|^OPERSPY [^ ]+![^ ]+@[^ ]+\{[^ ]+\} |]),
    (2, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is overriding |]),
    (2, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is using oper-override on |]),
    (2, "o", [str|^[^ ]+ \([^ ]+@[^ ]+\) is now an operator$|]),
    (1, "o", [str|^[^ ]+( \([^ ]+\))? dropped the nick |]),
    (1, "o", [str|^[^ ]+( \([^ ]+\))? dropped the account |]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? dropped the channel |]),
    (1, "o", [str|^[^ ]+( \([^ ]+\))? set vhost |]),
    (1, "o", [str|^[^ ]+( \([^ ]+\))? deleted vhost |]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? is using MODE |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? froze the account |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? thawed the account |]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? transferred foundership of #|]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? marked the channel #|]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? unmarked the channel #|]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? is forcing flags change |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? is clearing channel #|]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? closed the channel #|]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? reopened the channel #|]),
    (2, "o", [str|^[^ ]+( \([^ ]+\))? reopened #|]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? reset the password for the account|]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? enabled automatic klines on the channel|]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? disabled automatic klines on the channel|]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? forbade the nickname |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? unforbade the nickname |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? is removing oper class for |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? is changing oper class for |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? set the REGNOLIMIT option for the account |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? set the HOLD option for the account |]),
    (3, "o", [str|^[^ ]+( \([^ ]+\))? returned the account |]),
    (1, "o", [str|^Not kicking immune user |]),
    (1, "o", [str|^Not kicking oper |]),
    (1, "o", [str|^Overriding KICK from |]),
    (1, "o", [str|^Overriding REMOVE from |]),
    (1, "o", [str|^Server [^ ]+ split from |]),
    (3, "o", [str|^Netsplit [^ ]+ <->|]),
    (2, "o", [str|^Remote SQUIT|]),
    (3, "o", [str|^ssld error for |]),
    (1, "o", [str|^Finished synchronizing with network|]),
    (3, "o", [str|^Link [^ ]+ notable TS delta|]),
    (1, "o", [str|^End of burst \(emulated\) from |]),
    (2, "o", [str|^Link with [^ ]+ established: |]),
    (2, "o", [str|^Connection to [^ ]+ activated$|]),
    (2, "o", [str|^Attempt to re-introduce|]),
    (1, "o", [str|^Server [^ ]+ being introduced|]),
    (2, "o", [str|^Netjoin [^ ]+ <->|]),
    (2, "o", [str|^Error connecting to|]),
    (1, "o", [str|^[^ ]+![^ ]+@[^ ]+ is sending resvs and xlines|]),
    (3, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is changing the privilege set|]),
    (3, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is opering |]),
    (3, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is deopering |]),
    (3, "o", [str|^[^ ]+ is using DEHELPER |]),
    (3, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is clearing the nick delay table|]),
    (3, "o", [str|^Module |]),
    (3, "o", [str|^Cannot locate module |]),
    (2, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is adding a permanent X-Line for \[.*\]|]),
    (2, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} added X-Line for \[.*\]|]),
    (2, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} is adding a permanent RESV for \[.*\]|]),
    (2, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} added RESV for \[.*\]|]),
    (2, "o", [str|^[^ ]+![^ ]+@[^ ]+\{[^ ]+\} has removed the RESV for:|]),
    (3, "o", [str|^[^ ]+ is an idiot\. Dropping |]), -- someone k-lined *@*
    (3, "o", [str|^Rejecting email for |]), -- registering from a badmailed address won't trigger this, emailing broken?
    (3, "o", [str|^ERROR |]),
    (3, "o", [str|^No response from [^ ]+, closing link$|]),

    (1, "u", [str|^Too many global connections for [^ ]+![^ ]+@[^ ]+$|]),
    (0, "u", [str|^Invalid username: |]),
    (0, "u", [str|^HTTP Proxy disconnected: |]),
    (2, "u", [str|^Unauthorised client connection from |]),
    (2, "u", [str|^[^ ]+( \([^ ]+\))? sent the password for the MARKED account|]),
    (2, "u", [str|^Not restoring mark|])]
-- -}
