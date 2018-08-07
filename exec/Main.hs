{-|
Description : Entry-point of executable
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

Entry point into glirc2. This module sets up VTY and launches the client.
-}
module Main where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.List (nub)
import Data.Text (Text)
import System.Exit
import System.IO
import Graphics.Vty

import Client.Configuration
import Client.EventLoop
import Client.Options
import Client.State
import Client.State.Focus

import Exports ()

-- | Main action for IRC client
main :: IO ()
main =
  do opts <- getOptions
     let mbPath = view optConfigFile opts
     (path,cfg) <- loadConfiguration' mbPath
     withClientState path cfg $ \st0 ->
       withVty $ \vty ->
         do st1 <- clientStartExtensions    st0
            st2 <- initialNetworkLogic opts st1
            st3 <- updateTerminalSize vty   st2
            eventLoop vty st3

initialNetworkLogic :: Options -> ClientState -> IO ClientState
initialNetworkLogic opts st = addInitialNetworks (nub networks) st
  where
    networks
      | view optNoConnect opts = view optInitialNetworks opts
      | otherwise              = view optInitialNetworks opts ++ clientAutoconnects st

-- | Load configuration and handle errors along the way.
loadConfiguration' :: Maybe FilePath -> IO (FilePath, Configuration)
loadConfiguration' mbPath =
  do cfgRes <- loadConfiguration mbPath
     case cfgRes of
       Right x -> return x
       Left (ConfigurationReadFailed e) ->
         report "Failed to open configuration:" e
       Left (ConfigurationParseFailed p e) ->
         report ("Failed to parse configuration file: " ++ p) e
       Left (ConfigurationMalformed p e) ->
         report ("Malformed configuration file: " ++ p ++ "\n(try --config-format)") e
  where
    report problem msg =
      do hPutStrLn stderr problem
         hPutStrLn stderr msg
         exitFailure

-- | Create connections for the given networks.
-- Set the client focus to the first network listed.
addInitialNetworks ::
  [Text] {- networks -} ->
  ClientState           ->
  IO ClientState
addInitialNetworks [] st = return st
addInitialNetworks (n:ns) st =
  do st' <- foldM (flip (addConnection 0 Nothing)) st (n:ns)
     return $! set clientFocus (NetworkFocus n) st'

-- | Initialize a 'Vty' value and run a continuation. Shutdown the 'Vty'
-- once the continuation finishes.
withVty :: (Vty -> IO a) -> IO a
withVty = bracket (mkVty mempty{bracketedPasteMode = Just True}) shutdown
