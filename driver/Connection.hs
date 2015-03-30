{-# LANGUAGE OverloadedStrings #-}

-- | This module is responsible for creating 'Connection' values
-- for a particular server as specified by its 'ServerSettings'
module Connection
  ( -- * Settings
    ServerSettings(..)
  , ssHostName
  , ssPort
  , ssTls
  , ssTlsClientCert
  , ssTlsClientKey

  -- * Operations
  , connect
  , getRawIrcLine
  ) where

import Control.Lens
import Data.ByteString    (ByteString)
import Data.Default.Class (def)
import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import Data.Text.Lens     (unpacked)
import Data.X509          (CertificateChain(..))
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Data.X509.File     (readSignedObject, readKeyFile)
import Data.X509.Validation (validateDefault)
import Network.Connection
import Network.Socket     (HostName, PortNumber)
import Network.TLS
import Network.TLS.Extra  (ciphersuite_strong)
import System.X509        (getSystemCertificateStore)
import qualified Config
import qualified Config.Lens as Config
import qualified Data.ByteString.Char8 as B8

import ServerSettings

-- | This behaves like 'connectionGetLine' but it strips off the @'\r'@
-- IRC calls for 512 byte packets  I rounded off to 1024.
getRawIrcLine :: Connection -> IO ByteString
getRawIrcLine h =
  do b <- connectionGetLine 1024 h
     return (if B8.null b then b else B8.init b)
        -- empty lines will still fail, just later and nicely

buildConnectionParams :: Config.Value -> ServerSettings -> IO ConnectionParams
buildConnectionParams config args =
  do useSecure <- if view ssTls args
                     then fmap Just (buildTlsSettings config args)
                     else return Nothing
     return ConnectionParams
       { connectionHostname  = view ssHostName args
       , connectionPort      = ircPort args
       , connectionUseSecure = useSecure
       , connectionUseSocks  = Nothing
       }

ircPort :: ServerSettings -> PortNumber
ircPort args =
  case view ssPort args of
    Just p -> fromIntegral p
    Nothing | view ssTls args -> 6697
            | otherwise       -> 6667

buildCertificateStore :: Config.Value -> IO CertificateStore
buildCertificateStore config =
  do systemStore <- getSystemCertificateStore
     userCerts   <- traverse readSignedObject (configExtraCertificates config)
     let userStore = makeCertificateStore (concat userCerts)
     return (userStore <> systemStore)

configExtraCertificates :: Config.Value -> [FilePath]
configExtraCertificates =
  toListOf $ Config.key "server-certificates"
           . Config.list
           . folded
           . Config.text
           . unpacked

buildTlsSettings :: Config.Value -> ServerSettings -> IO TLSSettings
buildTlsSettings config args =
  do store      <- buildCertificateStore config
     clientCred <- loadClientCredentials args

     return $ TLSSettings $
       (defaultParamsClient (view ssHostName args) "")

       { clientSupported = def
           { supportedCiphers = ciphersuite_strong }

       , clientHooks = def
           { onCertificateRequest = \_ -> return clientCred
           , onServerCertificate  =
               if view ssTlsInsecure args
                  then (\_ _ _ _ -> return [])
                  else validateDefault
           }

       , clientShared = def
           { sharedCAStore = store }
       }


loadClientCredentials :: ServerSettings -> IO (Maybe (CertificateChain, PrivKey))
loadClientCredentials args =
  case view ssTlsClientCert args of
    Nothing       -> return Nothing
    Just certPath ->
      do cert  <- readSignedObject certPath
         keys  <- readKeyFile (fromMaybe certPath (view ssTlsClientKey args))
         case keys of
           [key] -> return (Just (CertificateChain cert, key))
           []    -> fail "No private keys found"
           _     -> fail "Too many private keys found"

connect :: Config.Value -> ServerSettings -> IO Connection
connect config args = do
  connectionContext <- initConnectionContext
  connectionParams  <- buildConnectionParams config args
  connectTo connectionContext connectionParams
