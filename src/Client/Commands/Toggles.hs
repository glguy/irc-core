{-# Language OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Client.Commands.Toggles
Description : View modality command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Toggles (togglesCommands) where

import Client.Commands.Docs (togglesDocs, cmdDoc)
import Client.Commands.TabCompletion (noClientTab)
import Client.Commands.Types
import Client.Configuration (EditMode(SingleLineEditor, MultiLineEditor), LayoutMode(OneColumn, TwoColumn))
import Client.State
import Control.Lens (over, set)

togglesCommands :: CommandSection
togglesCommands = CommandSection "View toggles"

  [ Command
      (pure "toggle-detail")
      (pure ())
      $(togglesDocs >>= cmdDoc "toggle-detail")
    $ ClientCommand cmdToggleDetail noClientTab

  , Command
      (pure "toggle-activity-bar")
      (pure ())
      $(togglesDocs >>= cmdDoc "toggle-activity-bar")
    $ ClientCommand cmdToggleActivityBar noClientTab

  , Command
      (pure "toggle-show-ping")
      (pure ())
      $(togglesDocs >>= cmdDoc "toggle-show-ping")
    $ ClientCommand cmdToggleShowPing noClientTab

  , Command
      (pure "toggle-metadata")
      (pure ())
      $(togglesDocs >>= cmdDoc "toggle-metadata")
    $ ClientCommand cmdToggleMetadata noClientTab

  , Command
      (pure "toggle-layout")
      (pure ())
      $(togglesDocs >>= cmdDoc "toggle-layout")
    $ ClientCommand cmdToggleLayout noClientTab

  , Command
      (pure "toggle-editor")
      (pure ())
      $(togglesDocs >>= cmdDoc "toggle-editor")
    $ ClientCommand cmdToggleEditor noClientTab

  , Command
      (pure "toggle-edit-lock")
      (pure ())
      $(togglesDocs >>= cmdDoc "toggle-edit-lock")
    $ ClientCommand cmdToggleEditLock noClientTab
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

cmdToggleEditor :: ClientCommand ()
cmdToggleEditor st _ = commandSuccess (over clientEditMode aux st)
  where
    aux SingleLineEditor = MultiLineEditor
    aux MultiLineEditor = SingleLineEditor

cmdToggleEditLock :: ClientCommand ()
cmdToggleEditLock st _ = commandSuccess (over clientEditLock not st)
