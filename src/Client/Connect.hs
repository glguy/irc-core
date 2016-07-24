{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Client.Connect
Description : Interface to the connection package
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module is responsible for creating 'Connection' values
for a particular server as specified by a 'ServerSettings'.
This involves setting up certificate stores an mapping
network settings from the client configuration into the
network connection library.
-}

module Client.Connect (withConnection) where

import Control.Lens
import Control.Exception  (bracket)
import Data.Default.Class (def)
import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import Data.X509          (CertificateChain(..))
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Data.X509.File     (readSignedObject, readKeyFile)
import Network.Connection
import Network.Socket     (PortNumber)
import Network.TLS
import Network.TLS.Extra  (ciphersuite_all)
import System.X509        (getSystemCertificateStore)

import Client.ServerSettings

buildConnectionParams :: ServerSettings -> IO ConnectionParams
buildConnectionParams args =
  do useSecure <- if view ssTls args
                     then fmap Just (buildTlsSettings args)
                     else return Nothing

     let proxySettings = view ssSocksHost args <&> \host ->
                           SockSettingsSimple
                             host
                             (view ssSocksPort args)

     return ConnectionParams
       { connectionHostname  = view ssHostName args
       , connectionPort      = ircPort args
       , connectionUseSecure = useSecure
       , connectionUseSocks  = proxySettings
       }

ircPort :: ServerSettings -> PortNumber
ircPort args =
  case view ssPort args of
    Just p -> fromIntegral p
    Nothing | view ssTls args -> 6697
            | otherwise       -> 6667

buildCertificateStore :: ServerSettings -> IO CertificateStore
buildCertificateStore args =
  do systemStore <- getSystemCertificateStore
     userCerts   <- traverse readSignedObject (view ssServerCerts args)
     let userStore = makeCertificateStore (concat userCerts)
     return (userStore <> systemStore)

buildTlsSettings :: ServerSettings -> IO TLSSettings
buildTlsSettings args =
  do store <- buildCertificateStore args

     let noValidation =
           ValidationCache
             (\_ _ _ -> return ValidationCachePass)
             (\_ _ _ -> return ())

     return $ TLSSettings ClientParams
       { clientWantSessionResume    = Nothing
       , clientUseMaxFragmentLength = Nothing
       , clientServerIdentification =
           error "buildTlsSettings: field initialized by connectTo"
       , clientUseServerNameIndication = True
       , clientShared = def
           { sharedCAStore = store
           , sharedValidationCache =
               if view ssTlsInsecure args then noValidation else def
           }
       , clientHooks = def
           { onCertificateRequest = \_ -> loadClientCredentials args }
       , clientSupported = def
           { supportedCiphers = ciphersuite_all }
       , clientDebug = def
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

connect :: ConnectionContext -> ServerSettings -> IO Connection
connect connectionContext args = do
  connectionParams <- buildConnectionParams args
  connectTo connectionContext connectionParams

-- | Create a new 'Connection' which will be closed when the continuation finishes.
withConnection :: ConnectionContext -> ServerSettings -> (Connection -> IO a) -> IO a
withConnection cxt settings = bracket (connect cxt settings) connectionClose
