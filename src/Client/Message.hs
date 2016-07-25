{-# Language TemplateHaskell #-}
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
  , _ExitBody

  -- * Client message operations
  , msgText
  ) where

import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (ZonedTime)
import           Irc.Message

data MessageBody = IrcBody !IrcMsg | ErrorBody !String | ExitBody

makePrisms ''MessageBody

data ClientMessage = ClientMessage
  { _msgNetwork :: !Text
  , _msgBody    :: !MessageBody
  , _msgTime    :: !ZonedTime
  }

makeLenses ''ClientMessage

-- | Compute a searchable text representation of the message
msgText :: MessageBody -> Text
msgText (IrcBody irc) = ircMsgText irc
msgText (ErrorBody str) = Text.pack str
msgText ExitBody = Text.empty
