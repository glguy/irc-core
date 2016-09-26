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
  , NetworkId
  , NetworkEvent(..)
  , createConnection
  , Client.Network.Async.send

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
import           Data.Time
import           Irc.RateLimit
import           Hookup


-- | Identifier used to match connection events to connections.
type NetworkId = Int

-- | Handle for a network connection
data NetworkConnection = NetworkConnection
  { connOutQueue :: !(TQueue ByteString)
  , connAsync    :: !(Async ())
  }

-- | The sum of incoming events from a network connection. All events
-- are annotated with a network ID matching that given when the connection
-- was created as well as the time at which the message was recieved.
data NetworkEvent
  -- | Event for successful connection to host
  = NetworkOpen  !NetworkId !ZonedTime
  -- | Event for a new recieved line (newline removed)
  | NetworkLine  !NetworkId !ZonedTime !ByteString
  -- | Final message indicating the network connection failed
  | NetworkError !NetworkId !ZonedTime !SomeException
  -- | Final message indicating the network connection finished
  | NetworkClose !NetworkId !ZonedTime

instance Show NetworkConnection where
  showsPrec p _ = showParen (p > 10)
                $ showString "NetworkConnection _"

-- | Exceptions used to kill connections manually.
data TerminationReason
  = PingTimeout      -- ^ sent when ping timer expires
  | ForcedDisconnect -- ^ sent when client commands force disconnect
  deriving Show

instance Exception TerminationReason where
  displayException PingTimeout      = "connection killed due to ping timeout"
  displayException ForcedDisconnect = "connection killed by client command"

-- | Schedule a message to be transmitted on the network connection.
-- These messages are sent unmodified. The message should contain a
-- newline terminator.
send :: NetworkConnection -> ByteString -> IO ()
send c msg = atomically (writeTQueue (connOutQueue c) msg)

-- | Force the given connection to terminate.
abortConnection :: TerminationReason -> NetworkConnection -> IO ()
abortConnection reason c = cancelWith (connAsync c) reason

-- | Initiate a new network connection according to the given 'ServerSettings'.
-- All events on this connection will be added to the given queue. The resulting
-- 'NetworkConnection' value can be used for sending outgoing messages and for
-- early termination of the connection.
createConnection ::
  Int {- ^ delay in seconds -} ->
  NetworkId {- ^ Identifier to be used on incoming events -} ->
  ServerSettings ->
  TQueue NetworkEvent {- Queue for incoming events -} ->
  IO NetworkConnection
createConnection delay network settings inQueue =
   do outQueue <- atomically newTQueue

      supervisor <- async $
                      threadDelay (delay * 1000000) >>
                      startConnection network settings inQueue outQueue

      -- Having this reporting thread separate from the supervisor ensures
      -- that canceling the supervisor with abortConnection doesn't interfere
      -- with carefully reporting the outcome
      forkIO $ do outcome <- waitCatch supervisor
                  case outcome of
                    Right{} -> recordNormalExit
                    Left e  -> recordFailure e

      return NetworkConnection
        { connOutQueue = outQueue
        , connAsync    = supervisor
        }
  where
    recordFailure :: SomeException -> IO ()
    recordFailure ex =
        do now <- getZonedTime
           atomically (writeTQueue inQueue (NetworkError network now ex))

    recordNormalExit :: IO ()
    recordNormalExit =
      do now <- getZonedTime
         atomically (writeTQueue inQueue (NetworkClose network now))


startConnection ::
  NetworkId ->
  ServerSettings ->
  TQueue NetworkEvent ->
  TQueue ByteString ->
  IO ()
startConnection network settings inQueue outQueue =
  do rate <- newRateLimit
               (view ssFloodPenalty settings)
               (view ssFloodThreshold settings)
     withConnection settings $ \h ->
       do reportNetworkOpen network inQueue
          withAsync (sendLoop h outQueue rate) $ \sender ->
            withAsync (receiveLoop network h inQueue) $ \receiver ->
              do res <- waitEitherCatch sender receiver
                 case res of
                   Left  Right{}  -> fail "PANIC: sendLoop returned"
                   Right Right{}  -> return ()
                   Left  (Left e) -> throwIO e
                   Right (Left e) -> throwIO e

reportNetworkOpen :: NetworkId -> TQueue NetworkEvent -> IO ()
reportNetworkOpen network inQueue =
  do now <- getZonedTime
     atomically (writeTQueue inQueue (NetworkOpen network now))

sendLoop :: Connection -> TQueue ByteString -> RateLimit -> IO ()
sendLoop h outQueue rate =
  forever $
    do msg <- atomically (readTQueue outQueue)
       tickRateLimit rate
       Hookup.send h msg

ircMaxMessageLength :: Int
ircMaxMessageLength = 512

receiveLoop :: NetworkId -> Connection -> TQueue NetworkEvent -> IO ()
receiveLoop network h inQueue =
  do msg <- recvLine h (2*ircMaxMessageLength)
     unless (B.null msg) $
       do now <- getZonedTime
          atomically (writeTQueue inQueue (NetworkLine network now (B.init msg)))
          receiveLoop network h inQueue
