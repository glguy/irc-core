{-# LANGUAGE TemplateHaskell #-}

module Irc.Core.Prisms where

import Control.Lens
import Irc.Core

makePrisms ''MsgFromServer
