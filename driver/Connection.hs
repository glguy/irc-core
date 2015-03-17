{-# LANGUAGE OverloadedStrings #-}
module Connection (connect, getRawIrcLine) where

import Control.Lens
import Data.ByteString    (ByteString)
import Data.Default.Class (def)
import Data.Maybe         (fromMaybe)
import Data.X509          (CertificateChain(..))
import Data.X509.File     (readSignedObject, readKeyFile)
import Data.X509.Validation (validateDefault)
import Network.Connection
import Network.Socket     (PortNumber)
import Network.TLS
import Network.TLS.Extra  (ciphersuite_strong)
import System.X509        (getSystemCertificateStore)
import qualified Data.ByteString.Char8 as B8

import CommandArgs

-- | This behaves like 'connectionGetLine' but it strips off the @'\r'@
-- IRC calls for 512 byte packets  I rounded off to 1024.
getRawIrcLine :: Connection -> IO ByteString
getRawIrcLine h =
  do b <- connectionGetLine 1024 h
     return (if B8.null b then b else B8.init b)
        -- empty lines will still fail, just later and nicely

buildConnectionParams :: CommandArgs -> IO ConnectionParams
buildConnectionParams args =
  do useSecure <- if view cmdArgTls args
                     then fmap Just (buildTlsSettings args)
                     else return Nothing
     return ConnectionParams
       { connectionHostname  = view cmdArgServer args
       , connectionPort      = ircPort args
       , connectionUseSecure = useSecure
       , connectionUseSocks  = Nothing
       }

ircPort :: CommandArgs -> PortNumber
ircPort args =
  case view cmdArgPort args of
    Just p -> fromIntegral p
    Nothing | view cmdArgTls args -> 6697
            | otherwise           -> 6667



buildTlsSettings :: CommandArgs -> IO TLSSettings
buildTlsSettings args =
  do store      <- getSystemCertificateStore
     clientCred <- loadClientCredentials args

     return $ TLSSettings $
       (defaultParamsClient (view cmdArgServer args) "")

       { clientSupported = def
           { supportedCiphers = ciphersuite_strong }

       , clientHooks = def
           { onCertificateRequest = \_ -> return clientCred
           , onServerCertificate  =
               if view cmdArgTlsInsecure args
                  then (\_ _ _ _ -> return [])
                  else validateDefault
           }

       , clientShared = def
           { sharedCAStore = store }
       }


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
