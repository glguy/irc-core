{-# Options_GHC -Wno-unused-do-bind #-}

{-|
Module      : Client.Network.Async
Description : Event-based network IO
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module creates network connections and thread to manage those connections.
Events on these connections will be written to a given event queue, and
outgoing messages are recieved on an incoming event queue.

These network connections are rate limited for outgoing messages per the
rate limiting algorithm given in the IRC RFC.

Incoming network event messages are assumed to be framed by newlines.

When a network connection terminates normally its final messages will be
'NetworkClose'. When it terminates abnormally its final message will be
'NetworkError'.

-}

module Client.Network.Async
  ( NetworkConnection
  , NetworkEvent(..)
  , createConnection
  , Client.Network.Async.send
  , Client.Network.Async.recv

  -- * Abort connections
  , abortConnection
  , TerminationReason(..)
  ) where

import           Client.Configuration.ServerSettings
import           Client.Network.Connect
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Foldable
import           Data.Time
import           Irc.RateLimit
import           Hookup


-- | Handle for a network connection
data NetworkConnection = NetworkConnection
  { connOutQueue :: !(TQueue ByteString)
  , connInQueue  :: !(TQueue NetworkEvent)
  , connAsync    :: !(Async ())
  }

-- | The sum of incoming events from a network connection. All events
-- are annotated with a network ID matching that given when the connection
-- was created as well as the time at which the message was recieved.
data NetworkEvent
  -- | Event for successful connection to host
  = NetworkOpen  !ZonedTime
  -- | Event for a new recieved line (newline removed)
  | NetworkLine  !ZonedTime !ByteString
  -- | Final message indicating the network connection failed
  | NetworkError !ZonedTime !SomeException
  -- | Final message indicating the network connection finished
  | NetworkClose !ZonedTime

instance Show NetworkConnection where
  showsPrec p _ = showParen (p > 10)
                $ showString "NetworkConnection _"

-- | Exceptions used to kill connections manually.
data TerminationReason
  = PingTimeout      -- ^ sent when ping timer expires
  | ForcedDisconnect -- ^ sent when client commands force disconnect
  | StsUpgrade       -- ^ sent when the client disconnects due to sts policy
  deriving Show

instance Exception TerminationReason where
  displayException PingTimeout      = "connection killed due to ping timeout"
  displayException ForcedDisconnect = "connection killed by client command"
  displayException StsUpgrade       = "connection killed by sts policy"

-- | Schedule a message to be transmitted on the network connection.
-- These messages are sent unmodified. The message should contain a
-- newline terminator.
send :: NetworkConnection -> ByteString -> IO ()
send c msg = atomically (writeTQueue (connOutQueue c) msg)

recv :: NetworkConnection -> STM [NetworkEvent]
recv = flushTQueue . connInQueue

-- | Force the given connection to terminate.
abortConnection :: TerminationReason -> NetworkConnection -> IO ()
abortConnection reason c = cancelWith (connAsync c) reason

-- | Initiate a new network connection according to the given 'ServerSettings'.
-- All events on this connection will be added to the given queue. The resulting
-- 'NetworkConnection' value can be used for sending outgoing messages and for
-- early termination of the connection.
createConnection ::
  Int {- ^ delay in seconds -} ->
  ServerSettings ->
  IO NetworkConnection
createConnection delay settings =
   do outQueue <- newTQueueIO
      inQueue  <- newTQueueIO

      supervisor <- async $
                      threadDelay (delay * 1000000) >>
                      startConnection settings inQueue outQueue

      let recordFailure :: SomeException -> IO ()
          recordFailure ex =
              do now <- getZonedTime
                 atomically (writeTQueue inQueue (NetworkError now ex))

          recordNormalExit :: IO ()
          recordNormalExit =
            do now <- getZonedTime
               atomically (writeTQueue inQueue (NetworkClose now))

      -- Having this reporting thread separate from the supervisor ensures
      -- that canceling the supervisor with abortConnection doesn't interfere
      -- with carefully reporting the outcome
      forkIO $ do outcome <- waitCatch supervisor
                  case outcome of
                    Right{} -> recordNormalExit
                    Left e  -> recordFailure e

      return NetworkConnection
        { connOutQueue = outQueue
        , connInQueue  = inQueue
        , connAsync    = supervisor
        }


startConnection ::
  ServerSettings ->
  TQueue NetworkEvent ->
  TQueue ByteString ->
  IO ()
startConnection settings inQueue outQueue =
  do rate <- newRateLimit
               (view ssFloodPenalty settings)
               (view ssFloodThreshold settings)
     withConnection settings $ \h ->
       do reportNetworkOpen inQueue
          withAsync (sendLoop h outQueue rate) $ \sender ->
            withAsync (receiveLoop h inQueue) $ \receiver ->
              do res <- waitEitherCatch sender receiver
                 case res of
                   Left  Right{}  -> fail "PANIC: sendLoop returned"
                   Right Right{}  -> return ()
                   Left  (Left e) -> throwIO e
                   Right (Left e) -> throwIO e

reportNetworkOpen :: TQueue NetworkEvent -> IO ()
reportNetworkOpen inQueue =
  do now <- getZonedTime
     atomically (writeTQueue inQueue (NetworkOpen now))

sendLoop :: Connection -> TQueue ByteString -> RateLimit -> IO ()
sendLoop h outQueue rate =
  forever $
    do msg <- atomically (readTQueue outQueue)
       tickRateLimit rate
       Hookup.send h msg

ircMaxMessageLength :: Int
ircMaxMessageLength = 512

receiveLoop :: Connection -> TQueue NetworkEvent -> IO ()
receiveLoop h inQueue =
  do mb <- recvLine h (2*ircMaxMessageLength)
     for_ mb $ \msg ->
       do unless (B.null msg) $ -- RFC says to ignore empty messages
            do now <- getZonedTime
               atomically $ writeTQueue inQueue
                          $ NetworkLine now msg
          receiveLoop h inQueue
