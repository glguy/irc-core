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

import           Client.Configuration.ServerSettings
import           Control.Applicative
import           Control.Exception  (bracket)
import           Control.Lens
import           Network.Socket     (PortNumber)
import           Hookup

buildConnectionParams :: ServerSettings -> ConnectionParams
buildConnectionParams args =
  let tlsParams = TlsParams
                    (view ssTlsClientCert args)
                    (view ssTlsClientKey  args <|> view ssTlsClientCert args)
                    (view ssTlsServerCert args)
                    (view ssTlsCiphers    args)

      family =
        case view ssProtocolFamily args of
          Nothing -> defaultFamily
          Just pf -> pf

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
    { cpFamily = family
    , cpHost  = view ssHostName args
    , cpPort  = ircPort args
    , cpTls   = useSecure
    , cpSocks = proxySettings
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
