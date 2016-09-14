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

  -- * Lenses
  , configDefaults
  , configServers
  , configPalette
  , configWindowNames
  , configNickPadding
  , configConfigPath
  , configMacros
  , configExtensions
  , configExtraHighlights
  , configUrlOpener
  , configIgnores
  , configActivityBar

  -- * Loading configuration
  , loadConfiguration

  -- * Resolving paths
  , resolveConfigurationPath
  ) where

import           Client.Image.Palette
import           Client.Configuration.Colors
import           Client.Configuration.ServerSettings
import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Commands.WordCompletion
import           Control.Exception
import           Control.Monad
import           Config
import           Config.FromConfig
import           Control.Lens hiding (List)
import           Data.Foldable (for_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import           Irc.Identifier (Identifier, mkId)
import           System.Directory
import           System.FilePath
import           System.IO.Error

-- | Top-level client configuration information. When connecting to a
-- server configuration from '_configServers' is used where possible,
-- otherwise '_configDefaults' is used.
data Configuration = Configuration
  { _configDefaults         :: ServerSettings -- ^ Default connection settings
  , _configServers          :: (HashMap Text ServerSettings) -- ^ Host-specific settings
  , _configPalette          :: Palette
  , _configWindowNames      :: Text -- ^ Names of windows, used when alt-jumping)
  , _configExtraHighlights  :: HashSet Identifier -- ^ Extra highlight nicks/terms
  , _configNickPadding      :: Maybe Integer -- ^ Padding of nicks
  , _configConfigPath       :: Maybe FilePath
        -- ^ manually specified configuration path, used for reloading
  , _configMacros           :: Recognizer Macro -- ^ command macros
  , _configExtensions       :: [FilePath] -- ^ paths to shared library
  , _configUrlOpener        :: Maybe FilePath -- ^ paths to url opening executable
  , _configIgnores          :: HashSet Identifier -- ^ initial ignore list
  , _configActivityBar      :: Bool -- ^ initially visibility of the activity bar
  }
  deriving Show

makeLenses ''Configuration

-- | Failure cases when loading a configuration file.
data ConfigurationFailure

  -- | Error message from reading configuration file
  = ConfigurationReadFailed String

  -- | Error message from parser or lexer
  | ConfigurationParseFailed String

  -- | Error message from loading parsed configuration
  | ConfigurationMalformed String
  deriving Show

-- | default instance
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

     case runConfigParser (parseConfiguration mbPath def rawcfg) of
       Left loadError -> throwIO (ConfigurationMalformed (Text.unpack loadError))
       Right cfg -> return cfg


parseConfiguration ::
  Maybe FilePath {- ^ optionally specified path to config -} ->
  ServerSettings {- ^ prepopulated default server settings -} ->
  Value ->
  ConfigParser Configuration
parseConfiguration _configConfigPath def = parseSections $

  do _configDefaults <- fromMaybe def
                    <$> sectionOptWith (parseServerSettings def) "defaults"

     _configServers  <- fromMaybe HashMap.empty
                    <$> sectionOptWith (parseServers _configDefaults) "servers"

     _configPalette <- fromMaybe defaultPalette
                    <$> sectionOptWith parsePalette "palette"

     _configWindowNames <- fromMaybe defaultWindowNames
                    <$> sectionOpt "window-names"

     _configMacros <- fromMaybe mempty
                    <$> sectionOptWith parseMacroMap "macros"

     _configExtensions <- fromMaybe []
                    <$> sectionOptWith (parseList parseString) "extensions"

     _configUrlOpener <- sectionOptWith parseString "url-opener"

     _configExtraHighlights <- maybe HashSet.empty HashSet.fromList
                    <$> sectionOptWith (parseList parseIdentifier) "extra-highlights"

     _configNickPadding <- sectionOpt "nick-padding"

     _configIgnores <- maybe HashSet.empty HashSet.fromList
                    <$> sectionOptWith (parseList parseIdentifier) "ignores"

     _configActivityBar <- fromMaybe False <$> sectionOpt  "activity-bar"

     for_ _configNickPadding (\padding ->
       when (padding < 0)
            (liftConfigParser $
               failure "nick-padding has to be a non negative number"))

     return Configuration{..}

parsePalette :: Value -> ConfigParser Palette
parsePalette = parseSectionsWith paletteHelper defaultPalette

paletteHelper :: Palette -> Text -> Value -> ConfigParser Palette
paletteHelper p k v =
  case k of
    "nick-colors" -> do xs <- Vector.fromList <$> parseList parseAttr v
                        when (null xs) (failure "Empty palette")
                        return $! set palNicks xs p
    _ | Just (Lens l) <- lookup k paletteMap ->
          do !attr <- parseAttr v
             return $! set l attr p
    _ -> failure "Unknown palette entry"

parseServers :: ServerSettings -> Value -> ConfigParser (HashMap Text ServerSettings)
parseServers def v =
  do sss <- parseList (parseServerSettings def) v
     return (HashMap.fromList [(serverSettingName ss, ss) | ss <- sss])
  where
    serverSettingName ss =
      fromMaybe (views ssHostName Text.pack ss)
                (view ssName ss)

parseServerSettings :: ServerSettings -> Value -> ConfigParser ServerSettings
parseServerSettings = parseSectionsWith parseServerSetting

parseServerSetting :: ServerSettings -> Text -> Value -> ConfigParser ServerSettings
parseServerSetting ss k v =
  case k of
    "nick"                -> setFieldWith   ssNicks parseNicks
    "username"            -> setField       ssUser
    "realname"            -> setField       ssReal
    "userinfo"            -> setField       ssUserInfo
    "password"            -> setFieldMb     ssPassword
    "sasl-username"       -> setFieldMb     ssSaslUsername
    "sasl-password"       -> setFieldMb     ssSaslPassword
    "hostname"            -> setFieldWith   ssHostName      parseString
    "port"                -> setFieldWithMb ssPort          parseNum
    "tls"                 -> setFieldWith   ssTls           parseUseTls
    "tls-client-cert"     -> setFieldWithMb ssTlsClientCert parseString
    "tls-client-key"      -> setFieldWithMb ssTlsClientKey  parseString
    "server-certificates" -> setFieldWith   ssServerCerts   (parseList parseString)
    "connect-cmds"        -> setFieldWith   ssConnectCmds   (parseList parseMacroCommand)
    "socks-host"          -> setFieldWithMb ssSocksHost     parseString
    "socks-port"          -> setFieldWith   ssSocksPort     parseNum
    "chanserv-channels"   -> setFieldWith   ssChanservChannels (parseList parseIdentifier)
    "flood-penalty"       -> setField       ssFloodPenalty
    "flood-threshold"     -> setField       ssFloodThreshold
    "message-hooks"       -> setField       ssMessageHooks
    "name"                -> setFieldMb     ssName
    "reconnect-attempts"  -> setField       ssReconnectAttempts
    "autoconnect"         -> setField       ssAutoconnect
    "nick-completion"     -> setFieldWith   ssNickCompletion parseNickCompletion
    _                     -> failure "Unknown setting"
  where
    setField   l = setFieldWith   l parseConfig
    setFieldMb l = setFieldWithMb l parseConfig

    setFieldWith l p =
      do x <- p v
         return $! set l x ss

    setFieldWithMb l p =
      do x <- p v
         return $! set l (Just x) ss

parseNicks :: Value -> ConfigParser (NonEmpty Text)
parseNicks (Text nick) = return (nick NonEmpty.:| [])
parseNicks (List xs) =
  do xs' <- parseList parseConfig (List xs)
     case xs' of
       [] -> failure "empty list"
       y:ys -> return (y NonEmpty.:| ys)
parseNicks _ = failure "expected text or list of text"

parseUseTls :: Value -> ConfigParser UseTls
parseUseTls (Atom "yes")          = return UseTls
parseUseTls (Atom "yes-insecure") = return UseInsecureTls
parseUseTls (Atom "no")           = return UseInsecure
parseUseTls _                     = failure "expected yes, yes-insecure, or no"

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

parseMacroMap :: Value -> ConfigParser (Recognizer Macro)
parseMacroMap v = fromCommands <$> parseList parseMacro v

parseMacro :: Value -> ConfigParser (Text, Macro)
parseMacro = parseSections $
  do name     <- sectionReq "name"
     spec     <- fromMaybe noMacroArguments
             <$> sectionOptWith parseMacroArguments "arguments"
     commands <- sectionReqWith (parseList parseMacroCommand) "commands"
     return (name, Macro spec commands)

parseMacroArguments :: Value -> ConfigParser MacroSpec
parseMacroArguments v =
  do txt <- parseConfig v
     case parseMacroSpecs txt of
       Nothing -> failure "bad macro argument specs"
       Just ex -> return ex

parseMacroCommand :: Value -> ConfigParser [ExpansionChunk]
parseMacroCommand v =
  do txt <- parseConfig v
     case parseExpansion txt of
       Nothing -> failure "bad macro line"
       Just ex -> return ex

parseNickCompletion :: Value -> ConfigParser WordCompletionMode
parseNickCompletion v =
  case v of
    Atom "default" -> return defaultNickWordCompleteMode
    Atom "slack"   -> return slackNickWordCompleteMode
    _              -> failure "expected default or slack"
