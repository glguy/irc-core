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

import           Client.Configuration
import           Client.EventLoop.Actions
import           Client.State
import           Control.Lens
import           Data.List
import           Data.Ord
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image
import           Graphics.Vty.Input

-- | Render the lines of a table showing all of the available digraph entries
keyMapLines ::
  ClientState {- ^ client state -} ->
  [Image]     {- ^ output lines -}
keyMapLines
  = renderEntries
  . keyMapEntries
  . view (clientConfig . configKeyMap)

renderEntries :: [([Modifier], Key, Action)] -> [Image]
renderEntries entries =
  [ resizeWidth keyColWidth key <|> act | (key,act) <- images ]

  where
    third (_,_,x) = x

    images =
      [ ( string defAttr (prettyModifierKey mods key)
        , text'  defAttr (actionName act            )
        )
      | (mods,key,act) <- sortBy (comparing third) entries
      ]

    keyColWidth = 1 + maximum (0 : map (imageWidth . fst) images)
