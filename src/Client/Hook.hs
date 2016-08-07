{-# Language TemplateHaskell #-}

{-|
Module      : Client.Hook
Description : Hooks into the operation of the client.
Copyright   : (c) Dan Doel, 2016
License     : ISC
Maintainer  : dan.doel@gmail.com

This module defines types for hooking into the operation of the client.

-}

module Client.Hook
  ( -- | * Message hook results
    MessageResult(..)
    -- | * Message hooks
  , MessageHook(..)
  , messageHookName
  , messageHookStateful
  , messageHookAction
  , applyMessageHooks
  ) where

import Control.Lens
import Data.Monoid
import Data.Text

import Irc.Message

data MessageResult
  = PassMessage
  | OmitMessage
  | RemapMessage IrcMsg

instance Monoid MessageResult where
  mempty = PassMessage
  PassMessage `mappend` r = r
  l `mappend` _ = l

maybeFromResult :: IrcMsg -> MessageResult -> Maybe IrcMsg
maybeFromResult original PassMessage = Just original
maybeFromResult _        OmitMessage = Nothing
maybeFromResult _ (RemapMessage new) = Just new

data MessageHook = MessageHook
  { _messageHookName     :: Text -- ^ Identifying name for the hook
  , _messageHookStateful :: Bool -- ^ Whether the remapping should affect client state
  , _messageHookAction   :: IrcMsg -> MessageResult
      -- ^ (Partial) message remapping action
  }

makeLenses ''MessageHook

-- | Apply the given message hooks to an 'IrcMsg'. The hooks are tried in
-- order until one handles the message.
applyMessageHooks :: [MessageHook] -> IrcMsg -> Maybe IrcMsg
applyMessageHooks hs msg =
  maybeFromResult msg $
    foldMap (\h -> view messageHookAction h msg) hs
