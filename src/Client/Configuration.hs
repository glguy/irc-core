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
  , ConfigurationFailure(..)
  , configDefaults
  , configServers
  , configPalette
  , configWindowNames

  -- * Loading configuration
  , loadConfiguration

  -- * Resolving paths
  , resolveConfigurationPath
  ) where

import           Client.Image.Palette
import           Client.Configuration.Colors
import           Client.ServerSettings
import           Control.Exception
import           Control.Monad
import           Config
import           Config.FromConfig
import           Control.Lens hiding (List)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Graphics.Vty.Attributes
import           Irc.Identifier (Identifier, mkId)
import           Network.Socket (HostName)
import           System.Directory
import           System.FilePath
import           System.IO.Error

-- | Top-level client configuration information. When connecting to a
-- server configuration from '_configServers' is used where possible,
-- otherwise '_configDefaults' is used.
data Configuration = Configuration
  { _configDefaults :: ServerSettings -- ^ Default connection settings
  , _configServers  :: (HashMap HostName ServerSettings) -- ^ Host-specific settings
  , _configPalette  :: Palette
  , _configWindowNames :: Text -- ^ Names of windows, used when alt-jumping
  }
  deriving Show

makeLenses ''Configuration

data ConfigurationFailure
  = ConfigurationParseFailed String
  | ConfigurationMalformed String
  | ConfigurationReadFailed String
  deriving Show

instance Exception ConfigurationFailure

defaultWindowNames :: Text
defaultWindowNames = "1234567890qwertyuiop!@#$%^&*()QWERTYUIOP"

-- | Uses 'getAppUserDataDirectory' to find @.glirc/config@
getOldConfigPath :: IO FilePath
getOldConfigPath =
  do dir <- getAppUserDataDirectory "glirc"
     return (dir </> "config")

-- | Uses 'getXdgDirectory' 'XdgConfig' to find @.config/glirc/config@
getNewConfigPath :: IO FilePath
getNewConfigPath =
  do dir <- getXdgDirectory XdgConfig "glirc"
     return (dir </> "config")

-- | Empty configuration file used when no path is specified
-- and the configuration file is missing.
emptyConfigFile :: Text
emptyConfigFile = "{}\n"

-- | Attempt to read a file using the given handler when
-- a file does not exist. On failure a 'ConfigurationReadFailed'
-- exception is throw.
readFileCatchNotFound ::
  FilePath {- ^ file to read -} ->
  (IOError -> IO Text) {- ^ error handler for not found case -} ->
  IO Text
readFileCatchNotFound path onNotFound =
  do res <- try (Text.readFile path)
     case res of
       Left e | isDoesNotExistError e -> onNotFound e
              | otherwise -> throwIO (ConfigurationReadFailed (show e))
       Right txt -> return txt

-- | Either read a configuration file from one of the default
-- locations, in which case no configuration found is equivalent
-- to an empty configuration, or from the specified file where
-- no configuration found is an error.
readConfigurationFile ::
  Maybe FilePath {- ^ just file or use default search paths -} ->
  IO Text
readConfigurationFile mbPath =
  case mbPath of

    Just path ->
      readFileCatchNotFound path $ \e ->
        throwIO (ConfigurationReadFailed (show e))

    Nothing ->
      do newPath <- getNewConfigPath
         readFileCatchNotFound newPath $ \_ ->
           do oldPath <- getOldConfigPath
              readFileCatchNotFound oldPath $ \_ ->
                return emptyConfigFile


-- | Load the configuration file defaulting to @~/.glirc/config@.
loadConfiguration ::
  Maybe FilePath {- ^ path to configuration file -} ->
  IO (Either ConfigurationFailure Configuration)
loadConfiguration mbPath = try $
  do file <- readConfigurationFile mbPath
     def  <- loadDefaultServerSettings

     rawcfg <-
       case parse file of
         Left parseError -> throwIO (ConfigurationParseFailed parseError)
         Right rawcfg -> return rawcfg

     case runConfigParser (parseConfiguration def rawcfg) of
       Left loadError -> throwIO (ConfigurationMalformed (Text.unpack loadError))
       Right cfg -> return cfg


parseConfiguration :: ServerSettings -> Value -> ConfigParser Configuration
parseConfiguration def = parseSections $

  do _configDefaults <- fromMaybe def
                    <$> sectionOptWith (parseServerSettings def) "defaults"

     _configServers  <- fromMaybe HashMap.empty
                    <$> sectionOptWith (parseServers _configDefaults) "servers"

     _configPalette <- fromMaybe defaultPalette
                    <$> sectionOptWith parsePalette "palette"

     _configWindowNames <- fromMaybe defaultWindowNames
                    <$> sectionOpt "window-names"

     return Configuration{..}

parsePalette :: Value -> ConfigParser Palette
parsePalette = parseSectionsWith paletteHelper defaultPalette

paletteHelper :: Palette -> Text -> Value -> ConfigParser Palette
paletteHelper p k v =
  case k of
    "nick-colors" -> do xs <- parseColors v
                        return $! set palNicks xs p

    "self"        -> setAttr palSelf
    "time"        -> setAttr palTime
    "meta"        -> setAttr palMeta
    "sigil"       -> setAttr palSigil
    "label"       -> setAttr palLabel
    "latency"     -> setAttr palLatency
    "error"       -> setAttr palError
    "textbox"     -> setAttr palTextBox
    "window-name" -> setAttr palWindowName
    "activity"    -> setAttr palActivity
    "mention"     -> setAttr palMention
    _             -> failure "Unknown palette entry"
  where
    setAttr l =
      do x <- parseColor v
         let !attr = withForeColor defAttr x
         return $! set l attr p

parseSectionsWith :: (a -> Text -> Value -> ConfigParser a) -> a -> Value -> ConfigParser a
parseSectionsWith p start s =
  case s of
    Sections xs -> foldM (\x (Section k v) -> extendLoc k (p x k v)) start xs
    _ -> failure "Expected sections"

parseServers :: ServerSettings -> Value -> ConfigParser (HashMap HostName ServerSettings)
parseServers def v =
  do ys <- parseList (parseServerSettings def) v
     return (HashMap.fromList [(view ssHostName ss, ss) | ss <- ys])

parseServerSettings :: ServerSettings -> Value -> ConfigParser ServerSettings
parseServerSettings = parseSectionsWith parseServerSetting

parseServerSetting :: ServerSettings -> Text -> Value -> ConfigParser ServerSettings
parseServerSetting ss k v =
  case k of
    "nick"                -> setField       ssNick
    "username"            -> setField       ssUser
    "realname"            -> setField       ssReal
    "userinfo"            -> setField       ssUserInfo
    "password"            -> setFieldMb     ssPassword
    "sasl-username"       -> setFieldMb     ssSaslUsername
    "sasl-password"       -> setFieldMb     ssSaslPassword
    "hostname"            -> setFieldWith   ssHostName      parseString
    "port"                -> setFieldWithMb ssPort          parseNum
    "tls"                 -> setFieldWith   ssTls           parseBoolean
    "tls-insecure"        -> setFieldWith   ssTlsInsecure   parseBoolean
    "tls-client-cert"     -> setFieldWithMb ssTlsClientCert parseString
    "tls-client-key"      -> setFieldWithMb ssTlsClientKey  parseString
    "server-certificates" -> setFieldWith   ssServerCerts   (parseList parseString)
    "connect-cmds"        -> setField       ssConnectCmds
    "socks-host"          -> setFieldWithMb ssSocksHost     parseString
    "socks-port"          -> setFieldWith   ssSocksPort     parseNum
    "chanserv-channels"   -> setFieldWith   ssChanservChannels (parseList parseIdentifier)
    "flood-penalty"       -> setField       ssFloodPenalty
    "flood-threshold"     -> setField       ssFloodThreshold
    "message-hooks"       -> setField       ssMessageHooks
    _                     -> failure "Unknown section"
  where
    setField   l = setFieldWith   l parseConfig
    setFieldMb l = setFieldWithMb l parseConfig

    setFieldWith l p =
      do x <- p v
         return $! set l x ss

    setFieldWithMb l p =
      do x <- p v
         return $! set l (Just x) ss

parseBoolean :: Value -> ConfigParser Bool
parseBoolean (Atom "yes") = return True
parseBoolean (Atom "no")  = return False
parseBoolean _            = failure "expected yes or no"

parseList :: (Value -> ConfigParser a) -> Value -> ConfigParser [a]
parseList p (List xs) = traverse p xs
parseList _ _         = failure "expected list"

parseNum :: Num a => Value -> ConfigParser a
parseNum v = fromInteger <$> parseConfig v

parseIdentifier :: Value -> ConfigParser Identifier
parseIdentifier v = mkId <$> parseConfig v

parseString :: Value -> ConfigParser String
parseString v = Text.unpack <$> parseConfig v

-- | Resolve relative paths starting at the home directory rather than
-- the current directory of the client.
resolveConfigurationPath :: FilePath -> IO FilePath
resolveConfigurationPath path
  | isAbsolute path = return path
  | otherwise = do home <- getHomeDirectory
                   return (home </> path)
