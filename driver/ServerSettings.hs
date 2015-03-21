{-# LANGUAGE TemplateHaskell #-}

module ServerSettings where

import Control.Lens
import Network.Socket     (HostName, PortNumber)

data ServerSettings = ServerSettings
  { _ssNick          :: String
  , _ssUser          :: String
  , _ssReal          :: String
  , _ssUserInfo      :: String
  , _ssPassword      :: Maybe String
  , _ssSaslCredential:: Maybe (String,String)
  , _ssHostName      :: HostName
  , _ssPort          :: Maybe PortNumber
  , _ssTls           :: Bool
  , _ssTlsInsecure   :: Bool
  , _ssTlsClientCert :: Maybe FilePath
  , _ssTlsClientKey  :: Maybe FilePath
  }

makeLenses ''ServerSettings
