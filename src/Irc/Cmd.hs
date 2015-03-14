{-# LANGUAGE OverloadedStrings #-}

-- | This module provides functions for constructing
-- outgoing IRC messages from the client to the server.
--
-- Note: These functions add the required trailing newline
-- characters.
module Irc.Cmd
  ( passCmd
  , nickCmd
  , userCmd
  , operCmd
  , modeCmd
  , quitCmd
  , joinCmd
  , partCmd
  , topicCmd
  , namesCmd
  , listCmd
  , inviteCmd
  , kickCmd
  , privMsgCmd
  , noticeCmd
  , whoisCmd
  , whowasCmd
  , whoCmd
  , pongCmd
  , capLsCmd
  , capReqCmd
  , capEndCmd
  , authenticateCmd
  , awayCmd
  , helpCmd
  , removeCmd
  , knockCmd
  , acceptCmd
  , timeCmd
  ) where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import qualified Data.ByteString.Char8 as B8

import Irc.Format

outgoingMsg :: RawIrcMsg
outgoingMsg = RawIrcMsg
  { msgTime = Nothing
  , msgPrefix = Nothing
  , msgCommand = ""
  , msgParams = []
  }

-- | Construct a MODE command
--
-- @MODE target *(mode) *(modeparams)@
modeCmd ::
  Identifier   {- ^ target -} ->
  [ByteString] {- ^ modes and params -} ->
  ByteString
modeCmd c modes = renderRawIrcMsg outgoingMsg
  { msgCommand  = "MODE"
  , msgParams = idBytes c : modes
  }

-- | Construct a KICK command
--
-- @KICK channel nick msg
kickCmd ::
  Identifier {- ^ channel -} ->
  Identifier {- ^ nick  -} ->
  ByteString {- ^ msg -} ->
  ByteString
kickCmd c nick msg = renderRawIrcMsg outgoingMsg
  { msgCommand  = "KICK"
  , msgParams = [idBytes c, idBytes nick, msg]
  }

-- | Construct a REMOVE command
--
-- @REMOVE channel nick msg
removeCmd ::
  Identifier {- ^ channel -} ->
  Identifier {- ^ nick  -} ->
  ByteString {- ^ msg -} ->
  ByteString
removeCmd c nick msg = renderRawIrcMsg outgoingMsg
  { msgCommand  = "REMOVE"
  , msgParams = [idBytes c, idBytes nick, msg]
  }

-- | Construct a JOIN command. A join command
-- can support multiple channels separated by
-- commas, and takes an optional channel key.
--
-- @JOIN channel [key]@
joinCmd :: Identifier -> Maybe ByteString -> ByteString
joinCmd chan mbKeys = renderRawIrcMsg outgoingMsg
  { msgCommand  = "JOIN"
  , msgParams = [idBytes chan] <> toList mbKeys
  }

-- | Construct a PART command.
--
-- @PART channel message@
partCmd ::
  Identifier {- ^ channel -} ->
  ByteString {- ^ message -} ->
  ByteString
partCmd chan msg = renderRawIrcMsg outgoingMsg
  { msgCommand  = "PART"
  , msgParams = [idBytes chan,msg]
  }

-- | Construct a TOPIC command. This is used to lookup
-- the current topic or to change it.
--
-- @TOPIC channel message@
topicCmd ::
  Identifier {- ^ channel -} ->
  ByteString {- ^ topic   -} ->
  ByteString
topicCmd chan msg = renderRawIrcMsg outgoingMsg
  { msgCommand  = "TOPIC"
  , msgParams = [idBytes chan,msg]
  }

-- | Construct a WHOIS command.
--
-- @WHOIS user@
whoisCmd ::
  Identifier {- ^ user -} ->
  ByteString
whoisCmd user = renderRawIrcMsg outgoingMsg
  { msgCommand  = "WHOIS"
  , msgParams = [idBytes user]
  }

-- | Construct a WHOWAS command.
--
-- @WHOWAS user@
whowasCmd ::
  Identifier {- ^ user -} ->
  ByteString
whowasCmd user = renderRawIrcMsg outgoingMsg
  { msgCommand  = "WHOWAS"
  , msgParams = [idBytes user]
  }

-- | Construct a NICK command. This is used to specify
-- the initial nickname as well as to change it.
--
-- @NICK nickname@
nickCmd ::
  Identifier {- ^ nickname -} ->
  ByteString
nickCmd nick = renderRawIrcMsg outgoingMsg
  { msgCommand = "NICK"
  , msgParams = [idBytes nick]
  }

-- | Construct a USER command. This is used in the initial
-- handshake to specify username and realname.
--
-- @USER username 0 * realname@
userCmd ::
  ByteString {- ^ username -} ->
  ByteString {- ^ realname -} ->
  ByteString
userCmd user realname = renderRawIrcMsg outgoingMsg
  { msgCommand = "USER"
  , msgParams = [user,"0","*",realname]
  }

-- | Construct a PONG command. This is used to respond to the PING
-- command to keep a connection alive.
--
-- @PONG token@
pongCmd ::
  ByteString {- ^ token -} ->
  ByteString
pongCmd token = renderRawIrcMsg outgoingMsg
  { msgCommand = "PONG"
  , msgParams = [token]
  }

-- | Construct a PASS command. This is used in the initial handshake
-- to specify a password for the connection.
--
-- @PASS password@
passCmd ::
  ByteString {- ^ password -} ->
  ByteString
passCmd password = renderRawIrcMsg outgoingMsg
  { msgCommand = "PASS"
  , msgParams = [password]
  }

-- | Construct a CAP LS command. This is used during the inital connection
-- to request a list of extensions that are supported by the server. It
-- should be followed by CAP REQ and eventually CAP END commands.
--
-- @CAP LS@
capLsCmd :: ByteString
capLsCmd = renderRawIrcMsg outgoingMsg
  { msgCommand = "CAP"
  , msgParams = ["LS"]
  }

-- | Construct a CAP REQ command. This is used to request a subset of
-- the capabilities returned in response to a CAP LS command.
--
-- @CAP REQ :cap0 cap1 .. capN@
capReqCmd :: [ByteString] -> ByteString
capReqCmd caps = renderRawIrcMsg outgoingMsg
  { msgCommand    = "CAP"
  , msgParams = ["REQ",B8.unwords caps]
  }

-- | Construct a CAP END command. This terminates the capability
-- negotiation portion of the initial connection.
--
-- @CAP END@
capEndCmd :: ByteString
capEndCmd = renderRawIrcMsg outgoingMsg
  { msgCommand    = "CAP"
  , msgParams = ["END"]
  }

-- | Construct a PRIVMSG command. This send normal chat messages
-- to both users as well as channels.
--
-- @PRIVMSG target message@
privMsgCmd ::
  Identifier {- ^ target  -} ->
  ByteString {- ^ message -} ->
  ByteString
privMsgCmd target msg = renderRawIrcMsg outgoingMsg
  { msgCommand = "PRIVMSG"
  , msgParams = [idBytes target,msg]
  }

-- | Construct a NOTICE command. This send notice chat messages
-- to both users as well as channels.
--
-- @NOTICE target message@
noticeCmd ::
  Identifier {- ^ target  -} ->
  ByteString {- ^ message -} ->
  ByteString
noticeCmd target msg = renderRawIrcMsg outgoingMsg
  { msgCommand = "NOTICE"
  , msgParams = [idBytes target,msg]
  }

-- | Construct an AUTHENTICATE command.
--
-- @AUTHENTICATE message@
authenticateCmd ::
  ByteString {- ^ message -} ->
  ByteString
authenticateCmd msg = renderRawIrcMsg outgoingMsg
  { msgCommand = "AUTHENTICATE"
  , msgParams = [msg]
  }

-- | Construct a HELP command.
--
-- @HELP topic@
helpCmd ::
  ByteString {- ^ topic -} ->
  ByteString
helpCmd msg = renderRawIrcMsg outgoingMsg
  { msgCommand = "HELP"
  , msgParams = [msg]
  }

-- | Construct an AWAY command.
--
-- @AWAY away_message@
awayCmd ::
  ByteString {- ^ message -} ->
  ByteString
awayCmd msg = renderRawIrcMsg outgoingMsg
  { msgCommand = "AWAY"
  , msgParams = [msg]
  }

-- | Construct a QUIT command.
--
-- @QUIT quit_message@
quitCmd ::
  ByteString {- ^ message -} ->
  ByteString
quitCmd msg = renderRawIrcMsg outgoingMsg
  { msgCommand = "QUIT"
  , msgParams = [msg]
  }

-- | Construct a LIST command.
--
-- @LIST <channel> *("," <channel>) @
listCmd ::
  [Identifier] {- ^ channels -} ->
  ByteString
listCmd chans = renderRawIrcMsg outgoingMsg
  { msgCommand = "LIST"
  , msgParams = [B8.intercalate "," (map idBytes chans)]
  }

-- | Construct a INVITE command.
--
-- @INVITE <nickanme> <channel>@
inviteCmd ::
  Identifier {- ^ nickname -} ->
  Identifier {- ^ channel  -} ->
  ByteString
inviteCmd nick chan = renderRawIrcMsg outgoingMsg
  { msgCommand = "INVITE"
  , msgParams = [idBytes nick,idBytes chan]
  }

-- | Construct a NAMES command.
--
-- @NAMES [ <channel> *("," <channel>)@
namesCmd ::
  [Identifier] {- ^ channels -} ->
  ByteString
namesCmd chans = renderRawIrcMsg outgoingMsg
  { msgCommand = "NAMES"
  , msgParams = if null chans
                  then []
                  else [B8.intercalate "," (map idBytes chans)]
  }

-- | Construct an OPER command.
--
-- @OPER <name> <password>@
operCmd ::
  ByteString {- ^ name -} ->
  ByteString {- ^ password -} ->
  ByteString
operCmd name pass = renderRawIrcMsg outgoingMsg
  { msgCommand = "OPER"
  , msgParams = [name,pass]
  }

-- | Construct a WHO command.
--
-- @WHO <mask>@
whoCmd ::
  ByteString {- ^ mask -} ->
  ByteString
whoCmd mask = renderRawIrcMsg outgoingMsg
  { msgCommand = "WHO"
  , msgParams = [mask]
  }

-- | Construct a KNOCK command.
--
-- @KNOCK <channel>@
knockCmd ::
  Identifier {- ^ channel -} ->
  ByteString
knockCmd chan = renderRawIrcMsg outgoingMsg
  { msgCommand = "KNOCK"
  , msgParams = [idBytes chan]
  }

-- | Construct an ACCEPT command.
--
-- @ACCEPT <nick>@
acceptCmd ::
  ByteString {- ^ nick, -nick, * -} ->
  ByteString
acceptCmd nick = renderRawIrcMsg outgoingMsg
  { msgCommand = "ACCEPT"
  , msgParams = [nick]
  }

-- | Construct an TIME command.
--
-- @TIME [<server>]>@
timeCmd ::
  Maybe ByteString {- ^ server -} ->
  ByteString
timeCmd server = renderRawIrcMsg outgoingMsg
  { msgCommand = "TIME"
  , msgParams = toList server
  }
