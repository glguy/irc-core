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
import System.IO
import System.Exit

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
main = do
  args <- getCommandArguments
  cfg  <- loadConfiguration' (view cmdArgConfigFile args)
  withVty $ \vty ->
    runInUnboundThread $ do
      initialClientState cfg vty >>= evalStateT (do
        addInitialNetworks (view cmdArgInitialNetworks args)
        forever $ eventLoop
       )

-- | Load configuration and handle errors along the way.
loadConfiguration' :: Maybe FilePath -> IO Configuration
loadConfiguration' path =
  do cfgRes <- loadConfiguration path
     case cfgRes of
       Right cfg -> return cfg
       Left (ConfigurationReadFailed e) ->
         report "Failed to open configuration:" e
       Left (ConfigurationParseFailed e) ->
         report "Failed to parse configuration:" e
       Left (ConfigurationMalformed e) ->
         report "Configuration malformed: " e
  where
    report problem msg =
      do hPutStrLn stderr problem
         hPutStrLn stderr msg
         exitFailure

-- | Create connections for all the networks on the command line.
-- Set the client focus to the first network listed.
addInitialNetworks :: (MonadIO m, MonadState ClientState m) => [NetworkName] -> m ()
addInitialNetworks networks = do
  traverse_ addConnection networks
  _head (assign clientFocus . NetworkFocus) networks
