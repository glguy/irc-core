{-# Language OverloadedStrings #-}

{-|
Module      : Client.Hooks
Description : Available hooks
Copyright   : (c) Dan Doel, 2016
License     : ISC
Maintainer  : dan.doel@gmail.com

-}

module Client.Hooks
  ( messageHooks
  ) where

import Data.Text
import Data.HashMap.Strict
import Client.Hook

import Client.Hook.Znc.Buffextras

messageHooks :: HashMap Text MessageHook
messageHooks = fromList
  [ ("buffextras", buffextrasHook False)
  , ("buffextras-debug", buffextrasHook True)
  ]
