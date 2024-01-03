{-# Language OverloadedStrings, TemplateHaskell #-}

{-|
Module      : Client.Commands.Docs
Description : Command documentation
Copyright   : (c) TheDaemoness 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module contains definitions for all of glirc's command docs.
-}
module Client.Commands.Docs
  ( cmdDoc
  , chanopDocs
  , chatDocs
  , clientDocs
  , integrationDocs
  , netDocs
  , operDocs
  , queriesDocs
  , togglesDocs
  , windowDocs
  ) where

import Client.Docs (Docs, loadDoc, lookupDoc, makeHeader)
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Syntax (lift)

cmdDoc :: Docs -> String -> Q Exp
cmdDoc docs key = lookupDoc (makeHeader "Description") ('/':key) docs

-- TODO: Replace each id with something that splits off the command name.

chanopDocs :: Docs
chanopDocs = $(loadDoc id "cmds_chanop" >>= lift)

chatDocs :: Docs
chatDocs = $(loadDoc id "cmds_chat" >>= lift)

clientDocs :: Docs
clientDocs = $(loadDoc id "cmds_client" >>= lift)

integrationDocs :: Docs
integrationDocs = $(loadDoc id "cmds_integration" >>= lift)

netDocs :: Docs
netDocs = $(loadDoc id "cmds_net" >>= lift)

operDocs :: Docs
operDocs = $(loadDoc id "cmds_oper" >>= lift)

queriesDocs :: Docs
queriesDocs = $(loadDoc id "cmds_queries" >>= lift)

togglesDocs :: Docs
togglesDocs = $(loadDoc id "cmds_toggles" >>= lift)

windowDocs :: Docs
windowDocs = $(loadDoc id "cmds_window" >>= lift)
