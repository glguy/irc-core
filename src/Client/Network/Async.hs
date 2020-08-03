{-# Options_GHC -Wno-unused-do-bind #-}
{-# Language OverloadedStrings #-}
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
  , upgrade

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
import           Data.List
import           Data.Void
import           Irc.RateLimit
import           Hookup
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word8)
import           Numeric (showHex)
import           OpenSSL.X509 (X509, printX509)


-- | Handle for a network connection
data NetworkConnection = NetworkConnection
  { connOutQueue :: TQueue ByteString
  , connInQueue  :: TQueue NetworkEvent
  , connAsync    :: Async ()
  , connUpgrade  :: MVar (IO ())
  }

-- | Signals that the server is ready to initiate the TLS handshake.
-- This is a no-op when not in a starttls state.
upgrade :: NetworkConnection -> IO ()
upgrade c = join (swapMVar (connUpgrade c) (pure ()))

-- | The sum of incoming events from a network connection. All events
-- are annotated with a network ID matching that given when the connection
-- was created as well as the time at which the message was recieved.
data NetworkEvent
  -- | Event for successful connection to host (certificate lines)
  = NetworkOpen  !ZonedTime [Text]
  -- | Event for a new recieved line (newline removed)
  | NetworkLine  !ZonedTime !ByteString
  -- | Report an error on network connection network connection failed
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
  | BadCertFingerprint ByteString (Maybe ByteString)
  | BadPubkeyFingerprint ByteString (Maybe ByteString)
  deriving Show

instance Exception TerminationReason where
  displayException PingTimeout      = "connection killed due to ping timeout"
  displayException ForcedDisconnect = "connection killed by client command"
  displayException StsUpgrade       = "connection killed by sts policy"
  displayException (BadCertFingerprint expect got) =
       "Expected certificate fingerprint: " ++ formatDigest expect ++
       "; got: "    ++ maybe "none" formatDigest got
  displayException (BadPubkeyFingerprint expect got) =
       "Expected public key fingerprint: " ++ formatDigest expect ++
       "; got: "    ++ maybe "none" formatDigest got

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

      upgradeMVar <- newEmptyMVar

      supervisor <- async $
                      threadDelay (delay * 1000000) >>
                      startConnection settings inQueue outQueue upgradeMVar

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
        , connUpgrade  = upgradeMVar
        }


startConnection ::
  ServerSettings ->
  TQueue NetworkEvent ->
  TQueue ByteString ->
  MVar (IO ()) ->
  IO ()
startConnection settings inQueue outQueue upgradeMVar =
     withConnection settings $ \h ->
       do reportNetworkOpen h inQueue
          case view ssTls settings of
            TlsStart -> starttls h
            _ -> do putMVar upgradeMVar (pure ()) -- make upgrade a no-op
                    finish h
  where
    -- Use the STARTTLS handshake. No sender thread will be started
    -- until the TLS session is established, but a receive loop is
    -- started to catch all messages up to the RPL_STARTTLS reply.
    starttls h =
      do Hookup.send h "STARTTLS\n"
         r <- withAsync (receiveLoop h inQueue) $ \t ->
                do putMVar upgradeMVar (cancel t)
                   waitCatch t
         case r of
           -- network connection closed
           Right () -> pure ()

           -- pre-receiver was killed by a call to 'upgrade'
           Left e | Just AsyncCancelled <- fromException e ->
              do Hookup.upgradeTls
                   (tlsParams settings)
                   (view ssHostName settings)
                   h
                 finish h

           -- something else went wrong with network IO
           Left e -> throwIO e

    -- Any TLS session that needs to be established has been,
    -- create the sender and permanent receiver thread
    finish h =
      do  for_ (view ssTlsCertFingerprint settings)
            (checkCertFingerprint h)
          for_ (view ssTlsPubkeyFingerprint settings)
            (checkPubkeyFingerprint h)

          res <- race (receiveLoop h inQueue)
                      (sendLoop h outQueue =<<
                       newRateLimit
                            (view ssFloodPenalty settings)
                            (view ssFloodThreshold settings))
          case res of
            Right v -> absurd v
            Left () -> pure ()

checkCertFingerprint :: Connection -> Fingerprint -> IO ()
checkCertFingerprint h fp =
  do (expect, got) <-
       case fp of
         FingerprintSha1   expect -> (,) expect <$> getPeerCertFingerprintSha1   h
         FingerprintSha256 expect -> (,) expect <$> getPeerCertFingerprintSha256 h
         FingerprintSha512 expect -> (,) expect <$> getPeerCertFingerprintSha512 h
     unless (Just expect == got)
       (throwIO (BadCertFingerprint expect got))

checkPubkeyFingerprint :: Connection -> Fingerprint -> IO ()
checkPubkeyFingerprint h fp =
  do (expect, got) <-
       case fp of
         FingerprintSha1   expect -> (,) expect <$> getPeerPubkeyFingerprintSha1   h
         FingerprintSha256 expect -> (,) expect <$> getPeerPubkeyFingerprintSha256 h
         FingerprintSha512 expect -> (,) expect <$> getPeerPubkeyFingerprintSha512 h
     unless (Just expect == got)
       (throwIO (BadPubkeyFingerprint expect got))

reportNetworkOpen :: Connection -> TQueue NetworkEvent -> IO ()
reportNetworkOpen h inQueue =
  do now <- getZonedTime
     mbServer <- getPeerCertificate h
     mbClient <- getClientCertificate h
     cTxts <- certText "Server" mbServer
     sTxts <- certText "Client" mbClient
     let txts = cTxts ++ sTxts
     atomically (writeTQueue inQueue (NetworkOpen now txts))

certText :: String -> Maybe X509 -> IO [Text]
certText label mbX509 =
  case mbX509 of
    Nothing -> pure []
    Just x509 ->
      do str <- printX509 x509
         return $! reverse (Text.lines (Text.pack ("<<" ++ label ++ ">>\n" ++ str)))

formatDigest :: ByteString -> String
formatDigest
  = intercalate ":"
  . map showByte
  . B.unpack

showByte :: Word8 -> String
showByte x
  | x < 0x10  = '0' : showHex x ""
  | otherwise = showHex x ""

sendLoop :: Connection -> TQueue ByteString -> RateLimit -> IO a
sendLoop h outQueue rate =
  forever $
  do msg <- atomically (readTQueue outQueue)
     tickRateLimit rate
     Hookup.send h msg

ircMaxMessageLength :: Int
ircMaxMessageLength = 512

receiveLoop :: Connection -> TQueue NetworkEvent -> IO ()
receiveLoop h inQueue =
  do mb <- recvLine h (4*ircMaxMessageLength)
     for_ mb $ \msg ->
       do unless (B.null msg) $ -- RFC says to ignore empty messages
            do now <- getZonedTime
               atomically $ writeTQueue inQueue
                          $ NetworkLine now msg
          receiveLoop h inQueue
