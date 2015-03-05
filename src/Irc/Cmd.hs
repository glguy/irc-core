{-# LANGUAGE OverloadedStrings #-}

-- | This module provides functions for constructing
-- outgoing IRC messages from the client to the server.
--
-- Note: These functions add the required trailing newline
-- characters.
module Irc.Cmd where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import qualified Data.ByteString.Char8 as B8

import Irc.Format

-- | Construct a MODE command
--
-- @MODE target *(mode) *(modeparams)@
modeCmd ::
  Identifier   {- ^ target -} ->
  ByteString   {- ^ modes  -} ->
  [ByteString] {- ^ params -} ->
  ByteString
modeCmd c modes params = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "MODE"
  , msgParams = idBytes c : modes : params
  }

-- | Construct a KICK command
--
-- @KICK channel nick msg
kickCmd ::
  Identifier {- ^ channel -} ->
  Identifier {- ^ nick  -} ->
  ByteString {- ^ msg -} ->
  ByteString
kickCmd c nick msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "KICK"
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
removeCmd c nick msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "REMOVE"
  , msgParams = [idBytes c, idBytes nick, msg]
  }

-- | Construct a JOIN command. A join command
-- can support multiple channels separated by
-- commas, and takes an optional channel key.
--
-- @JOIN channels (keys)@
joinCmd :: ByteString -> Maybe ByteString -> ByteString
joinCmd chans mbKeys = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "JOIN"
  , msgParams = [chans] <> toList mbKeys
  }

-- | Construct a PART command.
--
-- @PART channel message@
partCmd ::
  Identifier {- ^ channel -} ->
  ByteString {- ^ message -} ->
  ByteString
partCmd chan msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "PART"
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
topicCmd chan msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "TOPIC"
  , msgParams = [idBytes chan,msg]
  }

-- | Construct a WHOIS command.
--
-- @WHOIS user@
whoisCmd ::
  Identifier {- ^ user -} ->
  ByteString
whoisCmd user = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "WHOIS"
  , msgParams = [idBytes user]
  }

-- | Construct a WHOWAS command.
--
-- @WHOWAS user@
whowasCmd ::
  Identifier {- ^ user -} ->
  ByteString
whowasCmd user = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "WHOWAS"
  , msgParams = [idBytes user]
  }

-- | Construct a NICK command. This is used to specify
-- the initial nickname as well as to change it.
--
-- @NICK nickname@
nickCmd ::
  Identifier {- ^ nickname -} ->
  ByteString
nickCmd nick = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "NICK"
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
userCmd user realname = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "USER"
  , msgParams = [user,"0","*",realname]
  }

-- | Construct a PONG command. This is used to respond to the PING
-- command to keep a connection alive.
--
-- @PONG token@
pongCmd ::
  ByteString {- ^ token -} ->
  ByteString
pongCmd token = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "PONG"
  , msgParams = [token]
  }

-- | Construct a PASS command. This is used in the initial handshake
-- to specify a password for the connection.
--
-- @PASS password@
passCmd ::
  ByteString {- ^ password -} ->
  ByteString
passCmd password = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "PASS"
  , msgParams = [password]
  }

-- | Construct a CAP LS command. This is used during the inital connection
-- to request a list of extensions that are supported by the server. It
-- should be followed by CAP REQ and eventually CAP END commands.
--
-- @CAP LS@
capLsCmd :: ByteString
capLsCmd = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "CAP"
  , msgParams = ["LS"]
  }

-- | Construct a CAP REQ command. This is used to request a subset of
-- the capabilities returned in response to a CAP LS command.
--
-- @CAP REQ :cap0 cap1 .. capN@
capReqCmd :: [ByteString] -> ByteString
capReqCmd caps = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "CAP"
  , msgParams = ["REQ",B8.unwords caps]
  }

-- | Construct a CAP END command. This terminates the capability
-- negotiation portion of the initial connection.
--
-- @CAP END@
capEndCmd :: ByteString
capEndCmd = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "CAP"
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
privMsgCmd target msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "PRIVMSG"
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
noticeCmd target msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "NOTICE"
  , msgParams = [idBytes target,msg]
  }

-- | Construct an AUTHENTICATE command.
--
-- @AUTHENTICATE message@
authenticateCmd ::
  ByteString {- ^ message -} ->
  ByteString
authenticateCmd msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "AUTHENTICATE"
  , msgParams = [msg]
  }
