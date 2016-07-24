{-|
Module      : Main
Description : Entry-point of executable
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Entry point into glirc2. This module sets up VTY and launches the client.
-}

module Main where

import Control.Concurrent
import Control.Exception
import Data.Default.Class
import Graphics.Vty

import Client.EventLoop
import Client.Configuration
import Client.State

-- | Initialize a 'Vty' value and run a continuation. Shutdown the 'Vty'
-- once the continuation finishes.
withVty :: (Vty -> IO a) -> IO a
withVty = bracket (mkVty def) shutdown

-- | Main action for IRC client
main :: IO ()
main =
  do cfg <- loadConfiguration
     withVty $ \vty ->
       runInUnboundThread $
         do st <- initialClientState cfg vty
            eventLoop st
