{-# LANGUAGE TemplateHaskell #-}

module Client.ServerSettings where

import Control.Lens
import Data.Text (Text)
import System.Environment
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)

import Network.Socket (HostName, PortNumber)

data ServerSettings = ServerSettings
  { _ssNick          :: !Text
  , _ssUser          :: !Text
  , _ssReal          :: !Text
  , _ssUserInfo      :: !Text
  , _ssPassword      :: !(Maybe Text)
  , _ssSaslUsername  :: !(Maybe Text)
  , _ssSaslPassword  :: !(Maybe Text)
  , _ssHostName      :: !HostName
  , _ssPort          :: !(Maybe PortNumber)
  , _ssTls           :: !Bool
  , _ssTlsInsecure   :: !Bool
  , _ssTlsClientCert :: !(Maybe FilePath)
  , _ssTlsClientKey  :: !(Maybe FilePath)
  , _ssConnectCmds   :: ![Text]
  , _ssSocksHost     :: !(Maybe HostName)
  , _ssSocksPort     :: !PortNumber
  , _ssServerCerts   :: ![FilePath]
  }
  deriving Show

makeLenses ''ServerSettings

loadDefaultServerSettings :: IO ServerSettings
loadDefaultServerSettings =
  do env  <- getEnvironment
     let username = Text.pack (fromMaybe "guest" (lookup "USER" env))
     return ServerSettings
       { _ssNick          = username
       , _ssUser          = username
       , _ssReal          = username
       , _ssUserInfo      = username
       , _ssPassword      = Text.pack <$> lookup "IRCPASSWORD" env
       , _ssSaslUsername  = Nothing
       , _ssSaslPassword  = Text.pack <$> lookup "SASLPASSWORD" env
       , _ssHostName      = ""
       , _ssPort          = Nothing
       , _ssTls           = False
       , _ssTlsInsecure   = False
       , _ssTlsClientCert = Nothing
       , _ssTlsClientKey  = Nothing
       , _ssConnectCmds   = []
       , _ssSocksHost     = Nothing
       , _ssSocksPort     = 1080
       , _ssServerCerts   = []
       }
