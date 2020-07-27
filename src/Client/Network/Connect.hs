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
  , ircPort
  ) where

import           Client.Configuration.ServerSettings
import           Control.Applicative
import           Control.Exception  (bracket)
import           Control.Lens
import qualified Data.Text.Encoding as Text
import           Network.Socket (PortNumber)
import           Hookup

buildConnectionParams :: ServerSettings -> ConnectionParams
buildConnectionParams args =

  let tlsParams insecure = TlsParams
        { tpClientCertificate  = view ssTlsClientCert args
        , tpClientPrivateKey   = view ssTlsClientKey args <|> view ssTlsClientCert args
        , tpClientPrivateKeyPassword = privateKeyPassword
        , tpServerCertificate  = view ssTlsServerCert args
        , tpCipherSuite        = view ssTlsCiphers args
        , tpInsecure           = insecure
        }

      privateKeyPassword =
        case view ssTlsClientKeyPassword args of
          Just (SecretText str) -> PwBS (Text.encodeUtf8 str)
          _                     -> PwNone

      useSecure =
        case view ssTls args of
          UseInsecure    -> Nothing
          UseInsecureTls -> Just (tlsParams True)
          UseTls         -> Just (tlsParams False)

      proxySettings = view ssSocksHost args <&> \host ->
                        SocksParams
                          host
                          (view ssSocksPort args)

  in ConnectionParams
    { cpHost  = view ssHostName args
    , cpPort  = ircPort args
    , cpTls   = useSecure
    , cpSocks = proxySettings
    , cpBind  = view ssBindHostName args
    }


ircPort :: ServerSettings -> PortNumber
ircPort args =
  case view ssPort args of
    Just p -> fromIntegral p
    Nothing ->
      case view ssTls args of
        UseInsecure -> 6667
        _           -> 6697


-- | Create a new 'Connection' which will be closed when the continuation
-- finishes.
withConnection :: ServerSettings -> (Connection -> IO a) -> IO a
withConnection settings k =
  bracket (connect (buildConnectionParams settings)) close k
