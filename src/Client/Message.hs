{-# Language OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Client.Message
Description : Messages to be added to buffers
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the type used to track messages just before they
are added to a window.

-}
module Client.Message
  (
  -- * Client message type
    ClientMessage(..)
  , msgNetwork
  , msgBody
  , msgTime

  -- * Message body type
  , MessageBody(..)
  , _IrcBody
  , _ErrorBody
  , _NormalBody

  -- * Client message operations
  , IrcSummary(..)
  , msgSummary

  -- * Client message operations
  , msgText
  ) where

import           Control.Lens
import           Data.Text (Text)
import           Data.Time (ZonedTime)
import           Irc.Message
import           Irc.Identifier
import           Irc.UserInfo
import           Irc.Codes
import           Client.State.DCC (isSend)

data MessageBody
  = IrcBody    !IrcMsg
  | ErrorBody  {-# UNPACK #-} !Text
  | NormalBody {-# UNPACK #-} !Text

makePrisms ''MessageBody

data ClientMessage = ClientMessage
  { _msgNetwork :: !Text
  , _msgBody    :: !MessageBody
  , _msgTime    :: !ZonedTime
  }

makeLenses ''ClientMessage

data IrcSummary
  = JoinSummary {-# UNPACK #-} !Identifier
  | QuitSummary {-# UNPACK #-} !Identifier
  | PartSummary {-# UNPACK #-} !Identifier
  | NickSummary {-# UNPACK #-} !Identifier {-# UNPACK #-} !Identifier
  | ReplySummary {-# UNPACK #-} !ReplyCode
  | ChatSummary {-# UNPACK #-} !UserInfo
  | CtcpSummary {-# UNPACK #-} !Identifier
  | DccSendSummary {-# UNPACK #-} !Identifier
  | ChngSummary {-# UNPACK #-} !Identifier -- ^ Chghost command
  | AcctSummary {-# UNPACK #-} !Identifier -- ^ Account command
  | NoSummary
  deriving (Eq, Show)


-- | Compute a searchable text representation of the message
msgText :: MessageBody -> Text
msgText (IrcBody    irc) = ircMsgText irc
msgText (ErrorBody  txt) = txt
msgText (NormalBody txt) = txt


msgSummary :: MessageBody -> IrcSummary
msgSummary (IrcBody    irc) = ircSummary irc
msgSummary (ErrorBody  _  ) = NoSummary
msgSummary (NormalBody _  ) = NoSummary


ircSummary :: IrcMsg -> IrcSummary
ircSummary msg =
  case msg of
    Join who _ _ _  -> JoinSummary (userNick who)
    Part who _ _    -> PartSummary (userNick who)
    Quit who _      -> QuitSummary (userNick who)
    Nick who who'   -> NickSummary (userNick who) who'
    Privmsg who _ _ -> ChatSummary who
    Notice who _ _  -> ChatSummary who
    Ctcp who _ "ACTION" _ -> ChatSummary who
    Ctcp who _ "DCC" txt | isSend txt -> DccSendSummary (userNick who)
    Ctcp who _ _ _ -> CtcpSummary (userNick who)
    CtcpNotice who _ _ _ -> ChatSummary who
    Reply code _    -> ReplySummary code
    Account who _   -> AcctSummary (userNick who)
    Chghost who _ _ -> ChngSummary (userNick who)
    _               -> NoSummary
