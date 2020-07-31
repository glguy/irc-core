{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Toggles
Description : View modality command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Toggles (togglesCommands) where

import           Client.Commands.TabCompletion
import           Client.Commands.Types
import           Client.Configuration
import           Client.State
import           Control.Lens

togglesCommands :: CommandSection
togglesCommands = CommandSection "View toggles"

  [ Command
      (pure "toggle-detail")
      (pure ())
      "Toggle detailed message view.\n"
    $ ClientCommand cmdToggleDetail noClientTab

  , Command
      (pure "toggle-activity-bar")
      (pure ())
      "Toggle detailed detailed activity information in status bar.\n"
    $ ClientCommand cmdToggleActivityBar noClientTab

  , Command
      (pure "toggle-show-ping")
      (pure ())
      "Toggle visibility of ping round-trip time.\n"
    $ ClientCommand cmdToggleShowPing noClientTab

  , Command
      (pure "toggle-metadata")
      (pure ())
      "Toggle visibility of metadata in chat windows.\n"
    $ ClientCommand cmdToggleMetadata noClientTab

  , Command
      (pure "toggle-layout")
      (pure ())
      "Toggle multi-window layout mode.\n"
    $ ClientCommand cmdToggleLayout noClientTab

  ]

cmdToggleDetail :: ClientCommand ()
cmdToggleDetail st _ = commandSuccess (over clientDetailView not st)

cmdToggleActivityBar :: ClientCommand ()
cmdToggleActivityBar st _ = commandSuccess (over clientActivityBar not st)

cmdToggleShowPing :: ClientCommand ()
cmdToggleShowPing st _ = commandSuccess (over clientShowPing not st)

cmdToggleMetadata :: ClientCommand ()
cmdToggleMetadata st _ = commandSuccess (clientToggleHideMeta st)

cmdToggleLayout :: ClientCommand ()
cmdToggleLayout st _ = commandSuccess (set clientScroll 0 (over clientLayout aux st))
  where
    aux OneColumn = TwoColumn
    aux TwoColumn = OneColumn
