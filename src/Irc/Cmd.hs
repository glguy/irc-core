{-# LANGUAGE OverloadedStrings #-}
module Irc.Cmd where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Foldable (toList)

import Irc.Format

joinCmd :: ByteString -> Maybe ByteString -> ByteString
joinCmd chan mbKey = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand  = "JOIN"
  , msgParams = [chan] <> toList mbKey
  }

nickCmd :: ByteString -> ByteString
nickCmd nick = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "NICK"
  , msgParams = [nick]
  }

userCmd :: ByteString -> ByteString -> ByteString -> ByteString
userCmd user mode realname = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "USER"
  , msgParams = [user,mode,"*",realname]
  }

pongCmd :: ByteString -> ByteString
pongCmd token = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "PONG"
  , msgParams = [token]
  }

passCmd :: ByteString -> ByteString
passCmd password = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "PASS"
  , msgParams = [password]
  }

capLsCmd :: ByteString
capLsCmd = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "CAP"
  , msgParams = ["LS"]
  }

capReqCmd :: ByteString -> ByteString
capReqCmd caps = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "CAP"
  , msgParams = ["REQ",caps]
  }

capEndCmd :: ByteString
capEndCmd = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "CAP"
  , msgParams = ["END"]
  }

actionCmd :: ByteString -> ByteString -> ByteString
actionCmd target msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand    = "PRIVMSG"
  , msgParams = [target,"\SOHACTION " <> msg <> "\SOH"]
  }

privMsgCmd :: ByteString -> ByteString -> ByteString
privMsgCmd target msg = renderRawIrcMsg RawIrcMsg
  { msgPrefix = Nothing
  , msgCommand = "PRIVMSG"
  , msgParams = [target,msg]
  }

