{-# Language BlockArguments, TemplateHaskell, OverloadedStrings, BangPatterns #-}

{-|
Module      : Client.State.Target
Description : IRC message routing
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module contains glirc-specific overrides of the message routing provided by irc-core.
-}

module Client.State.Target
  (
    MessageTarget(..)
  , msgTarget
  ) where

import           Irc.Codes
import           Irc.Identifier (Identifier)
import           Irc.Message (IrcMsg(..), srcUser)
import qualified Irc.Message as Msg
import           Irc.UserInfo (userNick)

data MessageTarget
  = TargetDrop                 -- ^ Do not record the message anywhere.
  | TargetUser     !Identifier -- ^ Record the message in all channels/PMs shared with the user.
  | TargetWindow   !Identifier -- ^ Directed message to channel or from user.
  | TargetExisting !Identifier -- ^ As @TargetWindow@ but only for existing windows.
  | TargetNetwork              -- ^ Record the message in the network window.

msgTarget :: Identifier -> IrcMsg -> MessageTarget
msgTarget nick msg =
  case msg of
    Authenticate{} -> TargetDrop
    BatchStart{}   -> TargetDrop
    BatchEnd{}     -> TargetDrop
    Ping{}         -> TargetDrop
    Pong{}         -> TargetDrop
    Away user _    -> TargetExisting (userNick (srcUser user))
    Reply _ RPL_WHOSPCRPL [_,"616",_,_,_,_] -> TargetDrop
    _ -> case Msg.msgTarget nick msg of
      Msg.TargetUser id'   -> TargetUser id'
      Msg.TargetWindow id' -> TargetWindow id'
      Msg.TargetNetwork    -> TargetNetwork
