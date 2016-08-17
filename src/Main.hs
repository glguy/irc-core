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
import Data.Text (Text)
import Graphics.Vty
import System.Exit
import System.IO

import Client.CApi.Exports () -- foreign exports
import Client.CommandArguments
import Client.Configuration
import Client.EventLoop
import Client.State

-- | Initialize a 'Vty' value and run a continuation. Shutdown the 'Vty'
-- once the continuation finishes.
withVty :: (Vty -> IO a) -> IO a
withVty = bracket (mkVty def{bracketedPasteMode = Just True}) shutdown

-- | Main action for IRC client
main :: IO ()
main =
  do args <- getCommandArguments
     cfg  <- loadConfiguration' (view cmdArgConfigFile args)
     withVty $ \vty ->
       runInUnboundThread $
         initialClientState cfg vty >>=
         clientStartExtensions      >>=
         addInitialNetworks (view cmdArgInitialNetworks args) >>=
         eventLoop

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
addInitialNetworks ::
  [Text] {- networks -} ->
  ClientState           ->
  IO ClientState
addInitialNetworks networks st =
  case networks of
    []        -> return st
    network:_ ->
      do st' <- foldM (flip addConnection) st networks
         return (set clientFocus (NetworkFocus network) st')
