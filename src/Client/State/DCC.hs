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
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (bracket)
import           Control.Lens hiding (from)
import           Control.Monad (unless)
import           Data.Attoparsec.Text
import qualified Data.ByteString as B
import           Data.ByteString.Builder (word32BE, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.List (elem)
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


-- | Process the DCCOffer starting the exchange
startDownload :: DCCOffer -> IO ()
startDownload (DCCOffer from port name size) =
  bracket (connect param) close $ \conn ->
    bracket (IO.openFile (Text.unpack name) IO.AppendMode) IO.hClose
      $ \hdl -> do chan <- atomically newTChan -- Has to decouple send from recv
                   asId <- sendStream size conn chan
                   recvSendLoop 0 chan conn hdl
                   wait asId
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


-- | Thread `send`ing the current size to the fileserver. As an
--   independent acknowledgement stream, it doesn't match the protocol, but
--   matches what other clients and servers do in practice.
sendStream :: Word32 -> Connection -> TChan Word32 -> IO (Async ())
sendStream maxSize conn chan = async go
  where go :: IO ()
        go = do val <- atomically $ readTChan chan
                let valBE = toStrict . toLazyByteString $ word32BE val
                send conn valBE
                unless (val >= maxSize) go


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
