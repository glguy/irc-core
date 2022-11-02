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
  , summaryActor

  -- * Quit message details
  , QuitKind(..)

  -- * Client message operations
  , msgText
  ) where

import           Control.Lens
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Data.Time (ZonedTime)
import           Irc.Message
import           Irc.Identifier
import           Irc.UserInfo
import           Irc.Codes

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

data QuitKind
  = NormalQuit -- ^ User quit
  | MassQuit   -- ^ Mass event like a netsplit
  deriving (Eq, Show)

data IrcSummary
  = JoinSummary {-# UNPACK #-} !Identifier
  | QuitSummary {-# UNPACK #-} !Identifier !QuitKind
  | PartSummary {-# UNPACK #-} !Identifier
  | NickSummary {-# UNPACK #-} !Identifier {-# UNPACK #-} !Identifier
  | ReplySummary {-# UNPACK #-} !ReplyCode
  | ChatSummary {-# UNPACK #-} !UserInfo
  | CtcpSummary {-# UNPACK #-} !Identifier
  | ChngSummary {-# UNPACK #-} !Identifier -- ^ Chghost command
  | AcctSummary {-# UNPACK #-} !Identifier -- ^ Account command
  | AwaySummary {-# UNPACK #-} !Identifier !Bool
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
    Join who _ _ _  -> JoinSummary (userNick (srcUser who))
    Part who _ _    -> PartSummary (userNick (srcUser who))
    Quit who mbTxt  -> QuitSummary (userNick (srcUser who)) (quitKind mbTxt)
    Nick who who'   -> NickSummary (userNick (srcUser who)) who'
    Privmsg who _ _ -> ChatSummary (srcUser who)
    Notice who _ _  -> ChatSummary (srcUser who)
    Ctcp who _ "ACTION" _ -> ChatSummary (srcUser who)
    Ctcp who _ _ _ -> CtcpSummary (userNick (srcUser who))
    CtcpNotice who _ _ _ -> ChatSummary (srcUser who)
    Reply _ RPL_NOWAWAY (who:_) -> AwaySummary (mkId who) True
    Reply _ RPL_UNAWAY  (who:_) -> AwaySummary (mkId who) False
    Reply _ code _  -> ReplySummary code
    Account who _   -> AcctSummary (userNick (srcUser who))
    Chghost who _ _ -> ChngSummary (userNick (srcUser who))
    Away who mb     -> AwaySummary (userNick (srcUser who)) (isJust mb)
    _               -> NoSummary

quitKind :: Maybe Text -> QuitKind
quitKind mbReason =
  case mbReason of
    Just "*.net *.split"        -> MassQuit
    _                           -> NormalQuit

summaryActor :: IrcSummary -> Maybe Identifier
summaryActor s =
  case s of
    JoinSummary who   -> Just who
    QuitSummary who _ -> Just who
    PartSummary who   -> Just who
    NickSummary who _ -> Just who
    ChatSummary who   -> Just (userNick who)
    CtcpSummary who   -> Just who
    AcctSummary who   -> Just who
    ChngSummary who   -> Just who
    AwaySummary who _ -> Just who
    ReplySummary {}   -> Nothing
    NoSummary         -> Nothing
