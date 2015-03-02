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
  ByteString   {- ^ target           -} ->
  [ByteString] {- ^ modes and params -} ->
  ByteString
modeCmd c modes = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "MODE"
  , msgParams = c : modes
  }

-- | Construct a JOIN command. A join command
-- can support multiple channels separated by
-- commas, and takes an optional channel key.
--
-- @JOIN channels (keys)@
joinCmd :: ByteString -> Maybe ByteString -> ByteString
joinCmd chan mbKey = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "JOIN"
  , msgParams = [chan] <> toList mbKey
  }

-- | Construct a PART command.
--
-- @PART channel message@
partCmd ::
  ByteString {- ^ channel -} ->
  ByteString {- ^ message -} ->
  ByteString
partCmd chan msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "PART"
  , msgParams = [chan,msg]
  }

-- | Construct a TOPIC command. This is used to lookup
-- the current topic or to change it.
--
-- @TOPIC channel message@
topicCmd ::
  ByteString {- ^ channel -} ->
  ByteString {- ^ topic   -} ->
  ByteString
topicCmd chan msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "TOPIC"
  , msgParams = [chan,msg]
  }

-- | Construct a WHOIS command.
--
-- @WHOIS user@
whoisCmd ::
  ByteString {- ^ user -} ->
  ByteString
whoisCmd user = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "WHOIS"
  , msgParams = [user]
  }

-- | Construct a NICK command. This is used to specify
-- the initial nickname as well as to change it.
--
-- @NICK nickname@
nickCmd ::
  ByteString {- ^ nickname -} ->
  ByteString
nickCmd nick = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "NICK"
  , msgParams = [nick]
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
  ByteString {- ^ target  -} ->
  ByteString {- ^ message -} ->
  ByteString
privMsgCmd target msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "PRIVMSG"
  , msgParams = [target,msg]
  }

-- | Construct a NOTICE command. This send notice chat messages
-- to both users as well as channels.
--
-- @NOTICE target message@
noticeCmd ::
  ByteString {- ^ target  -} ->
  ByteString {- ^ message -} ->
  ByteString
noticeCmd target msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "NOTICE"
  , msgParams = [target,msg]
  }

