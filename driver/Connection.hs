{-# LANGUAGE OverloadedStrings #-}
module Connection (connect, getRawIrcLine) where

import Control.Lens
import Data.ByteString    (ByteString)
import Data.Default.Class (def)
import Data.Maybe         (fromMaybe)
import Data.X509          (CertificateChain(..))
import Data.X509.File     (readSignedObject, readKeyFile)
import Network.Connection
import Network.TLS
import Network.TLS.Extra  (ciphersuite_strong)
import System.X509        (getSystemCertificateStore)
import qualified Data.ByteString.Char8 as B8

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
      do store      <- getSystemCertificateStore
         clientCred <- loadClientCredentials args
         return (Just (TLSSettings (clientParams store clientCred)))
  | otherwise = return Nothing
  where
  clientParams store clientCred =
    (defaultParamsClient (view cmdArgServer args) "")
    { clientSupported = supported
    , clientHooks = hooks clientCred
    , clientShared = shared store
    }

  supported = def
    { supportedCiphers = ciphersuite_strong }

  hooks clientCred = def
    { onCertificateRequest = \_ -> return clientCred }

  shared store = def
    { sharedCAStore = store }

loadClientCredentials :: CommandArgs -> IO (Maybe (CertificateChain, PrivKey))
loadClientCredentials args =
  case view cmdArgTlsClientCert args of
    Nothing       -> return Nothing
    Just certPath ->
      do cert  <- readSignedObject certPath
         keys  <- readKeyFile (fromMaybe certPath (view cmdArgTlsClientKey args))
         case keys of
           [key] -> return (Just (CertificateChain cert, key))
           []    -> fail "No private keys found"
           _     -> fail "Too many private keys found"

connect :: CommandArgs -> IO Connection
connect args = do
  connectionContext <- initConnectionContext
  connectionParams  <- buildConnectionParams args
  connectTo connectionContext connectionParams
