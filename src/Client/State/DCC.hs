{-# Language TemplateHaskell, OverloadedStrings, BangPatterns
    , ScopedTypeVariables, TypeApplications #-}

{-|
Module      : Client.State.DCC
Description : CTCP DCC transfer handling
Copyright   : (c) Ruben Astudillo, 2019
License     : ISC
Maintainer  : ruben.astud@gmail.com

This module provides ADTs to hold a DCC offer made a by peer and
functions to start such transfer.
-}

module Client.State.DCC
  (
  -- * DCC offers
    DCCOffer(..)
  , dccFrom
  , dccPort
  , dccFileName
  , dccSize
  , startDownload
  , parseDCC
  -- * DCC transfer
  , DCCTransfer(..)
  , dtThread
  , dtProgressVar
  , dtProgress
  , pollProgress
  -- * DCC Update
  , DCCUpdate(..)
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception ( bracket, IOException
                                   , AsyncException, Exception(..))
import           Control.Lens hiding (from)
import           Control.Monad (unless)
import           Control.Monad (when)
import           Data.Attoparsec.Text
import qualified Data.ByteString as B
import           Data.ByteString.Builder (word32BE, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.IntMap (Key)
import           Data.List (elem)
import           Data.Maybe (isNothing)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           Hookup
import           Network.Socket ( HostName, PortNumber, Family(..)
                                , hostAddressToTuple )
import qualified System.IO as IO

-- | All the neccesary information to start the download
data DCCOffer = DCCOffer
  { _dccFrom     :: HostName -- ^ String of the ipv4 representation
  , _dccPort     :: PortNumber
  , _dccFileName :: Text
  , _dccSize     :: Word32 -- ^ Size of the whole file, per protocol restricted to 32-bits
  } deriving (Show, Eq)


makeLenses ''DCCOffer


data DCCTransfer = DCCTransfer
  { _dtThread      :: Maybe (Async ()) -- ^ If Nothing, the thread was killed
                                       --   and stopped.
  , _dtProgressVar :: TMVar Word32
  , _dtProgress    :: Word32 -- ^ Last _dtProgressVar seen
  }

makeLenses ''DCCTransfer


data DCCUpdate = PercentUpdate Key Word32
               | Finished Key
               | InterruptedTransfer Key
  deriving (Show, Eq)


-- | Process the DCCOffer starting the exchange
startDownload :: TMVar Word32 -> DCCOffer -> IO ()
startDownload progressVar (DCCOffer from port name totalSize) =
  bracket (connect param) close $ \conn ->
    -- BUG: do better that pure AppendMode
    bracket (IO.openFile (Text.unpack name) IO.AppendMode) IO.hClose $ \hdl ->
      do -- Has to decouple @send@ from @recv@, tells how much
         -- have we downloaded.
         recvChan1 <- atomically newTChan
         recvChan2 <- atomically $ dupTChan recvChan1

         -- Two threads, one for @send@ the progress to the
         -- server and another to signal how much progress
         -- have we done. `withAsync` guarrantee correct exception
         -- handling when the user cancels the transfer.
         withAsync (sendStream totalSize conn recvChan1) $ \outThread ->
           withAsync (report totalSize recvChan2 progressVar) $ \_reportThread ->
             do recvSendLoop 0 recvChan1 conn hdl
                wait outThread
  where
    param = ConnectionParams
              { cpFamily = AF_INET, cpHost = from
              , cpPort = port, cpSocks = Nothing
              , cpTls = Nothing }

    buffSize = 4 * 1024 * 1024

    recvSendLoop size chan conn hdl =
      do bytes <- recv conn buffSize
         unless (B.null bytes) $
           do B.hPut hdl bytes
              let newSize = size + fromIntegral (B.length bytes)
              atomically $ writeTChan chan newSize
              recvSendLoop newSize chan conn hdl


-- | `send`ing the current size to the fileserver. As an
--   independent acknowledgement stream, it doesn't match the protocol,
--   but matches what other clients and servers do in practice.
sendStream :: Word32 -> Connection -> TChan Word32 -> IO ()
sendStream maxSize conn chan =
  do val <- atomically $ readTChan chan
     let valBE = toStrict . toLazyByteString $ word32BE val
     send conn valBE
     unless (val >= maxSize) (sendStream maxSize conn chan)


report :: Word32 -> TChan Word32 -> TMVar Word32 -> IO ()
report totalSize input output = go 0
  where
    go :: Word32 -> IO ()
    go prevPercent =
      do curSize <- atomically $ readTChan input
         let -- curPercent :: Word64 so the (* 100) doesn't overflow.
             curPercent = percent curSize totalSize
         if (curPercent == 100)
           then atomically (putTMVar output curPercent)
           else do when (curPercent > prevPercent)
                        $ atomically (putTMVar output curPercent)
                   go curPercent

    percent :: Word32 -> Word32 -> Word32
    percent a total = fromIntegral $
      ((fromIntegral @_ @Word64 a) * 100) `div` (fromIntegral total)


parseDCC :: Text -> Either String DCCOffer
parseDCC = parseOnly dccFormat


dccFormat :: Parser DCCOffer
dccFormat =
      -- BUG: `name` maybe doesn't start with " or '.
  let sepName = choice [char '"', char '\'']
      fun name addr port size = DCCOffer addr port name size
  in fun <$> (string "SEND" *> space *> sepName
             *> takeWhile1 (\c -> not (c `elem` ['"', '\'']))
             <* sepName) <*> (ipv4Dotted <$> (space *> decimal))
             <*> (space *> decimal)
             <*> (space *> decimal)


ipv4Dotted :: Word32 -> HostName
ipv4Dotted addr =
  let bigToLittleEndian (a, b, c, d) = (d, c, b, a)
      ipv4Format (d,c,b,a) =
        show d <> "." <> show c <> "." <> show b <> "." <> show a
  in ipv4Format . bigToLittleEndian $ hostAddressToTuple addr

-- | select/poll on DCC progress
pollProgress :: [(Int, DCCTransfer)] -> STM DCCUpdate
pollProgress =
  let discern (key, trans) =
        do -- Pass over already finished transfer
           when (isNothing (_dtThread trans)) retry

           let Just threadId = _dtThread trans
           updateProgress key (_dtProgressVar trans)
             `orElse` finishDownload key threadId

      updateProgress :: Key -> TMVar Word32 -> STM DCCUpdate
      updateProgress key tmvar =
        do curProg <- takeTMVar tmvar
           return $ PercentUpdate key curProg

      finishDownload :: Key -> Async () -> STM DCCUpdate
      finishDownload key threadId = do
        res <- waitCatchSTM threadId
        case res of
          Right () -> return $ Finished key

          -- socket died
          Left exc | Just (_ :: IOException) <- fromException exc
                     -> return (InterruptedTransfer key)

          -- Compatibility hack
          -- user killed, thrown by async <= 2.1.1 on cancel
          Left exc | Just (_ :: AsyncException) <- fromException exc
                     -> return (InterruptedTransfer key)

          -- user killed, thrown by async >= 2.2
          Left exc | Just (_ :: AsyncCancelled ) <- fromException exc
                     -> return (InterruptedTransfer key)

          Left _ -> return (InterruptedTransfer key) -- catchall

  in getAlt . foldMap (Alt . discern)
