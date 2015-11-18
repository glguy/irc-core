{-# LANGUAGE TemplateHaskell #-}

module ServerSettings where

import Control.Lens
import Data.Text

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
  , _ssConnectCmds   :: [Text]
  , _ssSocksProxy    :: Maybe (HostName,PortNumber)
  , _ssServerCerts   :: [FilePath]
  }

makeLenses ''ServerSettings
