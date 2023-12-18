{-# Language OverloadedStrings #-}

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

import Language.Haskell.TH (Q, Exp)
import Client.Docs (Docs, loadDoc, lookupDoc, makeHeader)

cmdDoc :: String -> Docs -> Q Exp
cmdDoc key = lookupDoc (makeHeader "Description") ('/':key)

-- TODO: Replace each id with something that splits off the command name.

chanopDocs :: Q Docs
chanopDocs = loadDoc id "cmds_chanop"

chatDocs :: Q Docs
chatDocs = loadDoc id "cmds_chat"

clientDocs :: Q Docs
clientDocs = loadDoc id "cmds_client"

integrationDocs :: Q Docs
integrationDocs = loadDoc id "cmds_integration"

netDocs :: Q Docs
netDocs = loadDoc id "cmds_net"

operDocs :: Q Docs
operDocs = loadDoc id "cmds_oper"

queriesDocs :: Q Docs
queriesDocs = loadDoc id "cmds_queries"

togglesDocs :: Q Docs
togglesDocs = loadDoc id "cmds_toggles"

windowDocs :: Q Docs
windowDocs = loadDoc id "cmds_window"
