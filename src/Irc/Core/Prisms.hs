{-# LANGUAGE TemplateHaskell #-}

-- | Automatically generated 'Prism's for all of the types in 'MsgFromServer'
module Irc.Core.Prisms where

import Control.Lens
import Irc.Core

makePrisms ''MsgFromServer
