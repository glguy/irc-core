{-# LANGUAGE OverloadedStrings #-}
module Irc.Cmd where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Network.IRC.ByteString.Parser

joinCmd :: ByteString -> Maybe ByteString -> ByteString
joinCmd chan mbKey = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "JOIN"
  , msgParams = [chan] <> toList mbKey
  , msgTrail  = ""
  }

nickCmd :: ByteString -> ByteString
nickCmd nick = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "NICK"
  , msgParams = [nick]
  , msgTrail  = ""
  }

userCmd :: ByteString -> ByteString -> ByteString -> ByteString
userCmd user mode realname = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "USER"
  , msgParams = [user,mode,"*"]
  , msgTrail  = realname
  }

pongCmd :: ByteString -> ByteString
pongCmd token = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "PONG"
  , msgParams = []
  , msgTrail  = token
  }

passCmd :: ByteString -> ByteString
passCmd password = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "PASS"
  , msgParams = []
  , msgTrail  = password
  }

capLsCmd :: ByteString
capLsCmd = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "CAP"
  , msgParams = ["LS"]
  , msgTrail  = ""
  }

capReqCmd :: ByteString -> ByteString
capReqCmd caps = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "CAP"
  , msgParams = ["REQ"]
  , msgTrail  = caps
  }

capEndCmd :: ByteString
capEndCmd = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "CAP"
  , msgParams = ["END"]
  , msgTrail  = ""
  }

actionCmd :: ByteString -> ByteString -> ByteString
actionCmd target msg = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "PRIVMSG"
  , msgParams = [target]
  , msgTrail  = "\SOHACTION " <> msg <> "\SOH"
  }

privMsgCmd :: ByteString -> ByteString -> ByteString
privMsgCmd target msg = fromIRCMsg IRCMsg
  { msgPrefix = Nothing
  , msgCmd    = "PRIVMSG"
  , msgParams = [target]
  , msgTrail  = msg
  }

