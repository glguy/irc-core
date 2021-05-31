{-# Language OverloadedStrings #-}

{-|
Module      : Client.Hooks
Description : Available hooks
Copyright   : (c) Dan Doel, 2016
License     : ISC
Maintainer  : dan.doel@gmail.com

The collection of all hooks available in the client.

-}

module Client.Hooks
  ( messageHooks
  ) where

import Data.Text
import Data.HashMap.Strict
import Client.Hook

import Client.Hook.DroneBLRelay
import Client.Hook.Snotice
import Client.Hook.Znc.Buffextras

-- | All the available message hooks.
messageHooks :: HashMap Text ([Text] -> Maybe MessageHook)
messageHooks = fromList
  [ ("snotice"   , \_ -> Just snoticeHook)
  , ("frerelay"  , freRelayHook)
  , ("buffextras", buffextrasHook)
  ]
