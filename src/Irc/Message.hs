{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Irc.Message
  ( -- * High-level IRC events
    IrcMessage(..)
  , IrcMessageType(..)
  , mesgType
  , mesgSender
  , mesgStamp
  , mesgStatus
  , mesgMe
  , mesgModes
  , defaultIrcMessage

  -- * Prisms
  , _PrivMsgType
  , _NoticeMsgType
  , _ActionMsgType
  , _AwayMsgType
  , _JoinMsgType
  , _KickMsgType
  , _PartMsgType
  , _QuitMsgType
  , _NickMsgType
  , _TopicMsgType
  , _ErrorMsgType
  , _ErrMsgType
  , _ModeMsgType
  , _InviteMsgType
  , _KnockMsgType
  , _CallerIdMsgType
  , _CallerIdDeliveredMsgType
  , _CtcpReqMsgType
  , _CtcpRspMsgType
  ) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX

import Irc.Core
import Irc.Format

-- | 'IrcMessage' represents a high-level event to be communicated out
-- to the library user when something changes on a connection.
data IrcMessage = IrcMessage
  { _mesgType :: !IrcMessageType
  , _mesgSender :: !UserInfo
  , _mesgStamp :: !UTCTime
  , _mesgMe :: !Bool
  , _mesgModes :: String
  , _mesgStatus :: String -- for Statusmsg feature
  }
  deriving (Read, Show)

defaultIrcMessage :: IrcMessage
defaultIrcMessage = IrcMessage
  { _mesgType = PrivMsgType ""
  , _mesgSender = UserInfo "" Nothing Nothing
  , _mesgStamp = posixSecondsToUTCTime 0
  , _mesgMe = False
  , _mesgModes = ""
  , _mesgStatus = ""
  }

-- | Event types and associated fields used by 'IrcMessage'.
data IrcMessageType
  = PrivMsgType   Text
  | NoticeMsgType Text
  | ActionMsgType Text
  | AwayMsgType   Text
  | JoinMsgType
  | KickMsgType   Identifier Text
  | PartMsgType   Text
  | QuitMsgType   Text
  | NickMsgType   Identifier
  | TopicMsgType  Text
  | ErrorMsgType  Text
  | ErrMsgType  IrcError -- Family of various responses
  | ModeMsgType Bool Char ByteString
  | InviteMsgType
  | KnockMsgType
  | CallerIdMsgType
  | CallerIdDeliveredMsgType
  | CtcpReqMsgType ByteString ByteString -- ^ ctcp command and arguments
  | CtcpRspMsgType ByteString ByteString -- ^ ctcp command and arguments
  deriving (Read, Show)

makeLenses ''IrcMessage
makePrisms ''IrcMessageType
