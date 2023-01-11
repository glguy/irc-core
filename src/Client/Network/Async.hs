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

import Client.Configuration.ServerSettings
import Client.Network.Connect (withConnection, tlsParams)
import Control.Concurrent (MVar, swapMVar, threadDelay, forkIO, newEmptyMVar, putMVar)
import Control.Concurrent.Async (Async, async, cancel, cancelWith, race_, waitCatch, withAsync, AsyncCancelled(AsyncCancelled))
import Control.Concurrent.STM
import Control.Exception (SomeException, Exception(fromException, displayException), throwIO)
import Control.Lens (view)
import Control.Monad (join, when, forever, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (ZonedTime, getZonedTime)
import Data.Traversable (for)
import Data.Word (Word8)
import Hookup
import Hookup.OpenSSL (getPubKeyDer)
import Irc.RateLimit (RateLimit, newRateLimit, tickRateLimit)
import Numeric (showHex)
import OpenSSL.EVP.Digest qualified as Digest
import OpenSSL.X509 (X509, printX509, writeDerX509)


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
  = NetworkOpen  !ZonedTime
  -- | Event indicating TLS is in effect
  | NetworkTLS  [Text]
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
  | StartTLSFailed   -- ^ STARTTLS was expected by server had an error
  | BadCertFingerprint ByteString (Maybe ByteString)
  | BadPubkeyFingerprint ByteString (Maybe ByteString)
  deriving Show

instance Exception TerminationReason where
  displayException PingTimeout      = "connection killed due to ping timeout"
  displayException ForcedDisconnect = "connection killed by client command"
  displayException StsUpgrade       = "connection killed by sts policy"
  displayException StartTLSFailed   = "connection killed due to failed STARTTLS"
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
                      withConnection settings
                        (startConnection settings inQueue outQueue upgradeMVar)

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
  Connection ->
  IO ()
startConnection settings inQueue outQueue upgradeMVar h =
  do reportNetworkOpen inQueue
     ready <- presend
     when ready $
       do checkFingerprints
          race_ receiveMain sendMain
  where
    receiveMain = receiveLoop h inQueue

    sendMain =
      do rate <- newRateLimit (view ssFloodPenalty   settings)
                              (view ssFloodThreshold settings)
         sendLoop h outQueue rate

    presend =
      case view ssTls settings of
        TlsNo  -> True <$ putMVar upgradeMVar (pure ())
        TlsYes ->
          do txts <- describeCertificates h
             putMVar upgradeMVar (pure ())
             atomically (writeTQueue inQueue (NetworkTLS txts))
             pure True
        TlsStart ->
          do Hookup.send h "STARTTLS\n"
             r <- withAsync receiveMain $ \t ->
                    do putMVar upgradeMVar (cancel t)
                       waitCatch t
             case r of
               -- network connection closed
               Right () -> pure False

               -- pre-receiver was killed by a call to 'upgrade'
               Left e | Just AsyncCancelled <- fromException e ->
                  do Hookup.upgradeTls (tlsParams settings) (view ssHostName settings) h
                     txts <- describeCertificates h
                     atomically (writeTQueue inQueue (NetworkTLS txts))
                     pure True

               -- something else went wrong with network IO
               Left e -> throwIO e

    checkFingerprints =
      case view ssTls settings of
        TlsNo -> pure ()
        _ ->
          do for_ (view ssTlsCertFingerprint   settings) (checkCertFingerprint   h)
             for_ (view ssTlsPubkeyFingerprint settings) (checkPubkeyFingerprint h)

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

reportNetworkOpen :: TQueue NetworkEvent -> IO ()
reportNetworkOpen inQueue =
  do now <- getZonedTime
     atomically (writeTQueue inQueue (NetworkOpen now))

describeCertificates :: Connection -> IO [Text]
describeCertificates h =
  do mbServer <- getPeerCertificate h
     mbClient <- getClientCertificate h
     cTxts <- certText "Server" mbServer
     sTxts <- certText "Client" mbClient
     pure (reverse (cTxts ++ sTxts))

certText :: String -> Maybe X509 -> IO [Text]
certText label mbX509 =
  case mbX509 of
    Nothing -> pure []
    Just x509 ->
      do str <- printX509 x509
         fps <- getFingerprints x509
         pure $ map Text.pack
              $ ('\^B' : label)
              : map colorize (lines str ++ fps)
  where
    colorize x@(' ':_) = x
    colorize xs = "\^C07" ++ xs

getFingerprints :: X509 -> IO [String]
getFingerprints x509 =
  do certDer <- writeDerX509 x509
     spkiDer <- getPubKeyDer x509
     xss <- for ["sha1", "sha256", "sha512"] $ \alg ->
              do mb <- Digest.getDigestByName alg
                 pure $ case mb of
                   Nothing -> []
                   Just d ->
                       ("Certificate " ++ alg ++ " fingerprint:")
                     : fingerprintLines (Digest.digestLBS d certDer)
                    ++ ("SPKI " ++ alg ++ " fingerprint:")
                     : fingerprintLines (Digest.digestBS d spkiDer)
     pure (concat xss)

fingerprintLines :: ByteString -> [String]
fingerprintLines
  = map ("    "++)
  . chunksOf (16*3)
  . formatDigest

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
