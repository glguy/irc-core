{-# Options_GHC -Wno-unused-do-bind #-}

{-|
Module      : Client.NetworkConnection
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

module Client.NetworkConnection
  ( NetworkConnection
  , NetworkId
  , NetworkEvent(..)
  , createConnection
  , abortConnection
  , send
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Time
import           Network.Connection

import           Irc.RateLimit
import           Client.Connect
import           Client.ServerSettings

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
  = NetworkLine  !NetworkId !ZonedTime !ByteString
    -- ^ Event for a new recieved line (newline removed)
  | NetworkError !NetworkId !ZonedTime !SomeException
    -- ^ Final message indicating the network connection failed
  | NetworkClose !NetworkId !ZonedTime
    -- ^ Final message indicating the network connection finished

instance Show NetworkConnection where
  showsPrec p _ = showParen (p > 10)
                $ showString "NetworkConnection _"

-- | Schedule a message to be transmitted on the network connection.
-- These messages are sent unmodified. The message should contain a
-- newline terminator.
send :: NetworkConnection -> ByteString -> IO ()
send c msg = atomically (writeTQueue (connOutQueue c) msg)

-- | Force the given connection to terminate.
abortConnection :: NetworkConnection -> IO ()
abortConnection = cancel . connAsync

-- | Initiate a new network connection according to the given 'ServerSettings'.
-- All events on this connection will be added to the given queue. The resulting
-- 'NetworkConnection' value can be used for sending outgoing messages and for
-- early termination of the connection.
createConnection ::
  NetworkId {- ^ Identifier to be used on incoming events -} ->
  ConnectionContext ->
  ServerSettings ->
  TQueue NetworkEvent {- Queue for incoming events -} ->
  IO NetworkConnection
createConnection network cxt settings inQueue =
   do outQueue <- atomically newTQueue

      supervisor <- async (startConnection network cxt settings inQueue outQueue)

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
  ConnectionContext ->
  ServerSettings ->
  TQueue NetworkEvent ->
  TQueue ByteString ->
  IO ()
startConnection network cxt settings onInput outQueue =
  do rate <- newRateLimitDefault
     withConnection cxt settings $ \h ->
       withAsync (sendLoop h outQueue rate)      $ \sender ->
       withAsync (receiveLoop network h onInput) $ \receiver ->
         do res <- waitEitherCatch sender receiver
            case res of
              Left  Right{}  -> fail "PANIC: sendLoop returned"
              Right Right{}  -> return ()
              Left  (Left e) -> throwIO e
              Right (Left e) -> throwIO e

sendLoop :: Connection -> TQueue ByteString -> RateLimit -> IO ()
sendLoop h outQueue rate =
  forever $
    do msg <- atomically (readTQueue outQueue)
       tickRateLimit rate
       connectionPut h msg

ircMaxMessageLength :: Int
ircMaxMessageLength = 512

receiveLoop :: NetworkId -> Connection -> TQueue NetworkEvent -> IO ()
receiveLoop network h inQueue =
  do msg <- connectionGetLine ircMaxMessageLength h
     unless (B.null msg) $
       do now <- getZonedTime
          atomically (writeTQueue inQueue (NetworkLine network now (B.init msg)))
          receiveLoop network h inQueue
