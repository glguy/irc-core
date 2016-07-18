module Main where

import Control.Concurrent
import Control.Exception
import Data.Default.Class
import Graphics.Vty

import Client.EventLoop
import Client.Configuration
import Client.State
import Network.Connection

withVty :: (Vty -> IO a) -> IO a
withVty = bracket (mkVty def) shutdown

-- Runs logic outside of the main thread
startLogic :: Vty -> Configuration -> IO ()
startLogic vty cfg =
  do cxt <- initConnectionContext
     st  <- initialClientState cfg cxt vty
     eventLoop st

main :: IO ()
main =
  do cfg     <- loadConfiguration

     withVty $ \vty ->
       runInUnboundThread (startLogic vty cfg)
