module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Graphics.Vty

import Client.Event
import Client.EventLoop
import Client.Configuration
import Client.State
import Network.Connection

withVty :: (Vty -> IO a) -> IO a
withVty k =
  do config <- standardIOConfig
     bracket (mkVty config) shutdown k

createUserInputThread :: Vty -> Chan ClientEvent -> IO ()
createUserInputThread vty inQueue =
  do thread <- async $ forever $ do event <- nextEvent vty
                                    writeChan inQueue (VtyEvent event)
     link thread

createTimerThread :: Chan ClientEvent -> IO ()
createTimerThread inQueue =
  do thread <- async $ forever $ do threadDelay 1000000 -- 1 sec
                                    writeChan inQueue TimerEvent
     link thread

-- Runs logic outside of the main thread
startLogic :: Vty -> Configuration -> Chan ClientEvent -> IO ()
startLogic vty cfg inQueue =
  do thread <- async $
       do cxt <- initConnectionContext
          st  <- initialClientState cfg cxt vty inQueue
          eventLoop st

     wait thread

main :: IO ()
main =
  do cfg <- loadConfiguration
     inQueue <- newChan
     createTimerThread inQueue

     withVty $ \vty ->
       do createUserInputThread vty inQueue
          startLogic vty cfg inQueue
