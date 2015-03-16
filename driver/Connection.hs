{-# LANGUAGE OverloadedStrings #-}
module Connection (connect, getRawIrcLine) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Default.Class
import qualified Data.ByteString.Char8 as B8
import Network.Connection
import Network.TLS
import Network.TLS.Extra
import System.X509

import CommandArgs
-- | This behaves like 'connectionGetLine' but it strips off the @'\r'@
-- IRC calls for 512 packets but some IRCds might send more, I round off
-- to 1024.
getRawIrcLine :: Connection -> IO ByteString
getRawIrcLine h =
  do b <- connectionGetLine 1024 h
     return (B8.init b)

buildConnectionParams :: CommandArgs -> IO ConnectionParams
buildConnectionParams args =
  do  tlsSettings <- buildTlsSettings args
      return ConnectionParams
        { connectionHostname  = view cmdArgServer args
        , connectionPort      = fromIntegral (view cmdArgPort args)
        , connectionUseSecure = tlsSettings
        , connectionUseSocks  = Nothing
        }

buildTlsSettings :: CommandArgs -> IO (Maybe TLSSettings)
buildTlsSettings args
  | view cmdArgTls args =
      do store <- getSystemCertificateStore
         return (Just (TLSSettings (clientParams store)))
  | otherwise = return Nothing
  where
  clientParams store = (defaultParamsClient (view cmdArgServer args) "")
    { clientSupported = supported
    , clientHooks = hooks
    , clientShared = shared store
    }

  supported = def
    { supportedCiphers = ciphersuite_strong }

  hooks = def

  shared store = def
    { sharedCAStore = store }

connect :: CommandArgs -> IO Connection
connect args = do
  connectionContext <- initConnectionContext
  connectionParams  <- buildConnectionParams args
  connectTo connectionContext connectionParams
