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

import Client.Hook (MessageHook)
import Data.HashMap.Strict (HashMap, fromList)
import Data.Text (Text)

import Client.Hook.DroneBLRelay (droneblRelayHook)
import Client.Hook.Matterbridge (matterbridgeHook)
import Client.Hook.Snotice (snoticeHook)
import Client.Hook.Znc.Buffextras (buffextrasHook)

-- | All the available message hooks.
messageHooks :: HashMap Text ([Text] -> Maybe MessageHook)
messageHooks = fromList
  [ ("snotice", \_ -> Just snoticeHook)
  , ("droneblrelay", droneblRelayHook)
  , ("buffextras", buffextrasHook)
  , ("matterbridge", matterbridgeHook)
  ]
