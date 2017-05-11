{-# Language OverloadedStrings #-}
{-|
Module      : Client.View.KeyMap
Description : List of the current key map
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a view of the key bindings map

-}
module Client.View.KeyMap (keyMapLines) where

import           Client.EventLoop.Actions
import           Client.Configuration
import           Client.State
import           Control.Lens
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image

-- | Render the lines of a table showing all of the available digraph entries
keyMapLines ::
  ClientState {- ^ client state -} ->
  [Image]     {- ^ output lines -}
keyMapLines st
  = map (string defAttr . show)
  $ keyMapEntries keyMap
  where
    keyMap         = view (clientConfig . configKeyMap) st
