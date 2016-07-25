{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language BangPatterns #-}
{-# Language RecordWildCards #-}

{-|
Module      : Client.Configuration
Description : Client configuration format and operations
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the top-level configuration information for the client.
-}

module Client.Configuration
  (
  -- * Configuration type
    Configuration(..)
  , configDefaults
  , configServers

  -- * Loading configuration
  , loadConfiguration
  ) where

import           Client.ServerSettings
import           Control.Applicative
import           Control.Exception
import           Config
import           Config.FromConfig
import           Control.Lens hiding (List)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Traversable
import           Irc.Identifier (Identifier, mkId)
import           Network.Socket (HostName)
import           System.Directory
import           System.FilePath

-- | Top-level client configuration information. When connecting to a
-- server configuration from '_configServers' is used where possible,
-- otherwise '_configDefaults' is used.
data Configuration = Configuration
  { _configDefaults :: ServerSettings -- ^ Default connection settings
  , _configServers  :: HashMap HostName ServerSettings -- ^ Host-specific settings
  }
  deriving Show

makeLenses ''Configuration

data ConfigurationFailure
  = ConfigurationParseFailed String
  | ConfigurationMalformed Text
  deriving Show

instance Exception ConfigurationFailure

getConfigPath :: IO FilePath
getConfigPath =
  do dir <- getAppUserDataDirectory "glirc"
     return (dir </> "config")

-- | Load the configuration file defaulting to @~/.glirc/config@.
-- This action can throw 'IOError' and 'ConfigurationFailure'
-- exceptions.
loadConfiguration ::
  Maybe FilePath {- ^ path to configuration file -} ->
  IO Configuration
loadConfiguration mbPath =
  do path <- maybe getConfigPath return mbPath
     file <- Text.readFile path
     def  <- loadDefaultServerSettings
     case parse file of
       Left parseError -> throwIO (ConfigurationParseFailed parseError)
       Right rawcfg ->
         case runConfigParser (parseConfiguration def rawcfg) of
           Left loadError -> throwIO (ConfigurationMalformed loadError)
           Right cfg -> return cfg


parseConfiguration :: ServerSettings -> Value -> ConfigParser Configuration
parseConfiguration def = parseSections $

  do _configDefaults <- fromMaybe def
                    <$> sectionOptWith (parseServerSettings def) "defaults"

     _configServers  <- fromMaybe HashMap.empty
                    <$> sectionOptWith (parseServers _configDefaults) "servers"

     return Configuration{..}

parseServers :: ServerSettings -> Value -> ConfigParser (HashMap HostName ServerSettings)
parseServers def (List xs) =
  do ys <- traverse (parseServerSettings def) xs
     return (HashMap.fromList [(view ssHostName ss, ss) | ss <- ys])
parseServers _ _ = failure "expected list"

sectionOptString :: Text -> SectionParser (Maybe String)
sectionOptString key = fmap Text.unpack <$> sectionOpt key

sectionOptStrings :: Text -> SectionParser (Maybe [String])
sectionOptStrings key = fmap (fmap Text.unpack) <$> sectionOpt key

sectionOptNum :: Num a => Text -> SectionParser (Maybe a)
sectionOptNum key = fmap fromInteger <$> sectionOpt key

sectionOptIdentifiers :: Text -> SectionParser (Maybe [Identifier])
sectionOptIdentifiers key = fmap (fmap mkId) <$> sectionOpt key

parseServerSettings :: ServerSettings -> Value -> ConfigParser ServerSettings
parseServerSettings !def =
  parseSections $
    do _ssNick           <- fieldReq  ssNick          "nick"
       _ssUser           <- fieldReq  ssUser          "username"
       _ssReal           <- fieldReq  ssReal          "realname"
       _ssUserInfo       <- fieldReq  ssUserInfo      "userinfo"
       _ssPassword       <- field     ssPassword      "password"
       _ssSaslUsername   <- field     ssSaslUsername  "sasl-username"
       _ssSaslPassword   <- field     ssSaslPassword  "sasl-password"
       _ssHostName       <- fieldReq' ssHostName      (sectionOptString "hostname")
       _ssPort           <- field'    ssPort          (sectionOptNum "port")
       _ssTls            <- fieldReq' ssTls           (boolean "tls")
       _ssTlsInsecure    <- fieldReq' ssTlsInsecure   (boolean "tls-insecure")
       _ssTlsClientCert  <- field'    ssTlsClientCert (sectionOptString "tls-client-cert")
       _ssTlsClientKey   <- field'    ssTlsClientKey  (sectionOptString "tls-client-key")
       _ssConnectCmds    <- fieldReq  ssConnectCmds   "connect-cmds"
       _ssSocksHost      <- field'    ssSocksHost     (sectionOptString "socks-host")
       _ssSocksPort      <- fieldReq' ssSocksPort     (sectionOptNum "socks-port")
       _ssServerCerts    <- fieldReq' ssServerCerts   (sectionOptStrings "server-certificates")
       _ssChanservChannels <- fieldReq' ssChanservChannels (sectionOptIdentifiers "chanserv-channels")
       return ServerSettings{..}
  where
    field    l key = field'    l (sectionOpt key)
    fieldReq l key = fieldReq' l (sectionOpt key)

    fieldReq' l p = fromMaybe (view l def) <$> p

    field' l p = (<|> view l def) <$> p

boolean :: Text -> SectionParser (Maybe Bool)
boolean key =
  do mb <- sectionOpt key
     for mb $ \a ->
       case atomName a of
         "yes" -> return True
         "no"  -> return False
         _     -> liftConfigParser (failure "expected yes or no")
