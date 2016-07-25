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
import Control.Lens
import Control.Monad
import Data.Default.Class
import Graphics.Vty

import Client.EventLoop
import Client.Configuration
import Client.CommandArguments
import Client.State

-- | Initialize a 'Vty' value and run a continuation. Shutdown the 'Vty'
-- once the continuation finishes.
withVty :: (Vty -> IO a) -> IO a
withVty = bracket (mkVty def) shutdown

-- | Main action for IRC client
main :: IO ()
main =
  do args <- getCommandArguments
     cfg <- loadConfiguration (view cmdArgConfigFile args)
     withVty $ \vty ->
       runInUnboundThread $
         do st <- initialClientState cfg vty
            st' <- addInitialNetworks (view cmdArgInitialNetworks args) st
            eventLoop st'

-- | Create connections for all the networks on the command line.
-- Set the client focus to the first network listed.
addInitialNetworks :: [NetworkName] -> ClientState -> IO ClientState
addInitialNetworks networks st =
  case networks of
    []        -> return st
    network:_ ->
      do st' <- foldM (flip addConnection) st networks
         return (set clientFocus (NetworkFocus network) st')
