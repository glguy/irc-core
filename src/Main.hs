module Main where

import Control.Concurrent
import Control.Exception
import Data.Default.Class
import Graphics.Vty

import Client.EventLoop
import Client.Configuration
import Client.State

withVty :: (Vty -> IO a) -> IO a
withVty = bracket (mkVty def) shutdown

main :: IO ()
main =
  do cfg <- loadConfiguration
     withVty $ \vty ->
       runInUnboundThread $
         do st <- initialClientState cfg vty
            eventLoop st
