{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Client.Network.Connect
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

module Client.Network.Connect
  ( withConnection
  ) where

import           Client.Configuration
import           Client.Configuration.ServerSettings
import           Control.Exception  (bracket)
import           Control.Lens
import           Control.Monad
import           Data.Default.Class (def)
import           Data.Monoid        ((<>))
import           Data.X509          (CertificateChain(..))
import           Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import           Data.X509.File     (readSignedObject, readKeyFile)
import           Network.Connection
import           Network.Socket     (PortNumber)
import           Network.TLS
import           Network.TLS.Extra  (ciphersuite_strong)
import           System.X509        (getSystemCertificateStore)

buildConnectionParams :: ServerSettings -> IO ConnectionParams
buildConnectionParams args =
  do useSecure <- case view ssTls args of
                    UseInsecure -> return Nothing
                    _           -> Just <$> buildTlsSettings args

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
    Nothing ->
      case view ssTls args of
        UseInsecure -> 6667
        _           -> 6697

buildCertificateStore :: ServerSettings -> IO CertificateStore
buildCertificateStore args =
  do systemStore <- getSystemCertificateStore
     userCerts   <- traverse (readSignedObject <=< resolveConfigurationPath)
                             (view ssServerCerts args)
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
       , clientUseServerNameIndication = False
       , clientShared = def
           { sharedCAStore = store
           , sharedValidationCache =
               case view ssTls args of
                 UseInsecureTls -> noValidation
                 _              -> def
           }
       , clientHooks = def
           { onCertificateRequest = \_ -> loadClientCredentials args }
       , clientSupported = def
           { supportedCiphers = ciphersuite_strong }
       , clientDebug = def
       }

loadClientCredentials :: ServerSettings -> IO (Maybe (CertificateChain, PrivKey))
loadClientCredentials args =
  case view ssTlsClientCert args of
    Nothing       -> return Nothing
    Just certPath ->
      do certPath' <- resolveConfigurationPath certPath
         cert      <- readSignedObject certPath'

         keyPath   <- case view ssTlsClientKey args of
                        Nothing      -> return certPath'
                        Just keyPath -> resolveConfigurationPath keyPath
         keys  <- readKeyFile keyPath
         case keys of
           [key] -> return (Just (CertificateChain cert, key))
           []    -> fail "No private keys found"
           _     -> fail "Too many private keys found"

-- | Create a new 'Connection' which will be closed when the continuation finishes.
withConnection :: ConnectionContext -> ServerSettings -> (Connection -> IO a) -> IO a
withConnection cxt settings k =
  do params <- buildConnectionParams settings
     bracket (connectTo cxt params) connectionClose k
