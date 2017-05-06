{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ApplicativeDo     #-}

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
  , configIndentWrapped
  , configConfigPath
  , configMacros
  , configExtensions
  , configExtraHighlights
  , configUrlOpener
  , configIgnores
  , configActivityBar
  , configBellOnMention

  -- * Loading configuration
  , loadConfiguration

  -- * Resolving paths
  , resolveConfigurationPath
  ) where

import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Commands.WordCompletion
import           Client.Configuration.Colors
import           Client.Configuration.ServerSettings
import           Client.Image.Palette
import           Config
import           Config.Schema
import           Control.Applicative
import           Control.Exception
import           Control.Lens                        hiding (List)
import           Data.Foldable                       (find, foldl')
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.HashSet                        (HashSet)
import qualified Data.HashSet                        as HashSet
import           Data.List.NonEmpty                  (NonEmpty)
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.Maybe
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.IO                        as Text
import qualified Data.Vector                         as Vector
import           Irc.Identifier                      (Identifier, mkId)
import           System.Directory
import           System.FilePath
import           System.IO.Error

-- | Top-level client configuration information. When connecting to a
-- server configuration from '_configServers' is used where possible,
-- otherwise '_configDefaults' is used.
data Configuration = Configuration
  { _configDefaults        :: ServerSettings -- ^ Default connection settings
  , _configServers         :: (HashMap Text ServerSettings) -- ^ Host-specific settings
  , _configPalette         :: Palette
  , _configWindowNames     :: Text -- ^ Names of windows, used when alt-jumping)
  , _configExtraHighlights :: HashSet Identifier -- ^ Extra highlight nicks/terms
  , _configNickPadding     :: Maybe Integer -- ^ Padding of nicks
  , _configIndentWrapped   :: Maybe Int -- ^ How far to indent wrapped lines
  , _configConfigPath      :: Maybe FilePath
        -- ^ manually specified configuration path, used for reloading
  , _configMacros          :: Recognizer Macro -- ^ command macros
  , _configExtensions      :: [FilePath] -- ^ paths to shared library
  , _configUrlOpener       :: Maybe FilePath -- ^ paths to url opening executable
  , _configIgnores         :: HashSet Identifier -- ^ initial ignore list
  , _configActivityBar     :: Bool -- ^ initially visibility of the activity bar
  , _configBellOnMention   :: Bool -- ^ notify terminal on mention
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

     case loadSections (configurationSpec mbPath def) rawcfg of
       Left loadError -> throwIO (ConfigurationMalformed (show loadError)) -- XXX show
       Right cfg -> return cfg


configurationSpec ::
  Maybe FilePath {- ^ optionally specified path to config -} ->
  ServerSettings {- ^ prepopulated default server settings -} ->
  SectionsSpec Configuration
configurationSpec _configConfigPath def =

  do ssDefUpdate <- fromMaybe id <$> optSection' "defaults" "" serverSpec
     ssUpdates   <- fromMaybe [] <$> optSection' "servers" "" (listSpec serverSpec)

     _configPalette <- fromMaybe defaultPalette
                    <$> optSection' "palette" "" (sectionsSpec paletteSpec)

     _configWindowNames <- fromMaybe defaultWindowNames
                    <$> optSection "window-names" ""

     _configMacros <- fromMaybe mempty
                    <$> optSection' "macros" "" macroMapSpec

     _configExtensions <- fromMaybe [] <$> optSection' "extensions" "" (listSpec filePathSpec)

     _configUrlOpener <- optSection' "url-opener" "" stringSpec

     _configExtraHighlights <- maybe HashSet.empty (HashSet.fromList . map mkId)
                    <$> optSection "extra-highlights" ""

     _configNickPadding <- optSection' "nick-padding" "" nonnegativeSpec

     _configIndentWrapped <- optSection' "indent-wrapped-lines" "" nonnegativeSpec

     _configIgnores <- maybe HashSet.empty (HashSet.fromList . map mkId)
                    <$> optSection "ignores" ""

     _configActivityBar <- fromMaybe False
                    <$> optSection' "activity-bar" "" yesOrNo

     _configBellOnMention <- fromMaybe False <$> optSection' "bell-on-mention" "" yesOrNo

     return (let _configDefaults = ssDefUpdate def
                 _configServers  = buildServerMap _configDefaults ssUpdates
             in Configuration{..})


nonnegativeSpec :: (Ord a, Num a) => ValuesSpec a
nonnegativeSpec = customSpec "non-negative" numSpec $ \x -> find (0 <=) [x]

filePathSpec :: ValuesSpec FilePath
filePathSpec = stringSpec


-- | Matches the 'yes' and 'no' atoms
yesOrNo :: ValuesSpec Bool
yesOrNo = True  <$ atomSpec "yes" <|>
          False <$ atomSpec "no"


paletteSpec :: SectionsSpec Palette
paletteSpec =
  do updates <- catMaybes <$> sequenceA
       [ fmap (set l) <$> optSection' lbl "" attrSpec | (lbl, Lens l) <- paletteMap ]
     nickColors <- optSection' "nick-colors" "" (nonemptyList attrSpec)
     return (let pal1 = foldl' (\acc f -> f acc) defaultPalette updates
             in case nickColors of
                  Nothing -> pal1
                  Just xs -> set palNicks (Vector.fromList (NonEmpty.toList xs)) pal1)

nonemptyList :: ValuesSpec a -> ValuesSpec (NonEmpty a)
nonemptyList s = customSpec "non-empty" (listSpec s) NonEmpty.nonEmpty


buildServerMap :: ServerSettings -> [ServerSettings -> ServerSettings] -> HashMap Text ServerSettings
buildServerMap def ups =
  HashMap.fromList [ (serverSettingName ss, ss) | up <- ups, let ss = up def ]
  where
    serverSettingName ss =
      fromMaybe (views ssHostName Text.pack ss)
                (view ssName ss)

serverSpec :: ValuesSpec (ServerSettings -> ServerSettings)
serverSpec = sectionsSpec $
  do updates <- catMaybes <$> sequenceA settings
     return (foldr (.) id updates)
  where
    req l s = set l         <$> s

    opt l s = set l Nothing <$  atomSpec "clear"
          <|> set l . Just  <$> s

    settings =
      [ optSection' "name"               "" $ opt ssName              valuesSpec
      , optSection' "nick"               "" $ req ssNicks             nicksSpec
      , optSection' "username"           "" $ req ssUser              valuesSpec
      , optSection' "realname"           "" $ req ssReal              valuesSpec
      , optSection' "userinfo"           "" $ req ssUserInfo          valuesSpec
      , optSection' "password"           "" $ opt ssPassword          valuesSpec
      , optSection' "sasl-username"      "" $ opt ssSaslUsername      valuesSpec
      , optSection' "sasl-password"      "" $ opt ssSaslPassword      valuesSpec
      , optSection' "sasl-ecdsa-key"     "" $ opt ssSaslEcdsaFile     filePathSpec
      , optSection' "hostname"           "" $ req ssHostName          filePathSpec
      , optSection' "port"               "" $ opt ssPort              numSpec
      , optSection' "tls"                "" $ req ssTls               useTlsSpec
      , optSection' "tls-client-cert"    "" $ opt ssTlsClientCert     filePathSpec
      , optSection' "tls-client-key"     "" $ opt ssTlsClientKey      filePathSpec
      , optSection' "tls-server-cert"    "" $ opt ssTlsServerCert     filePathSpec
      , optSection' "tls-ciphers"        "" $ req ssTlsCiphers        filePathSpec
      , optSection' "connect-cmds"       "" $ req ssConnectCmds       $ listSpec macroCommandSpec
      , optSection' "socks-host"         "" $ opt ssSocksHost         stringSpec
      , optSection' "socks-port"         "" $ req ssSocksPort         numSpec
      , optSection' "chanserv-channels"  "" $ req ssChanservChannels  $ listSpec identifierSpec
      , optSection' "flood-penalty"      "" $ req ssFloodPenalty      valuesSpec
      , optSection' "flood-threshold"    "" $ req ssFloodThreshold    valuesSpec
      , optSection' "message-hooks"      "" $ req ssMessageHooks      valuesSpec
      , optSection' "reconnect-attempts" "" $ req ssReconnectAttempts valuesSpec
      , optSection' "autoconnect"        "" $ req ssAutoconnect       yesOrNo
      , optSection' "nick-completion"    "" $ req ssNickCompletion    nickCompletionSpec
      , optSection' "log-dir"            "" $ opt ssLogDir            filePathSpec
      ]


nicksSpec :: ValuesSpec (NonEmpty Text)
nicksSpec = pure <$> valuesSpec <|> nonemptyList valuesSpec


useTlsSpec :: ValuesSpec UseTls
useTlsSpec =
  UseTls         <$ atomSpec "yes" <|>
  UseInsecureTls <$ atomSpec "yes-insecure" <|>
  UseInsecure    <$ atomSpec "no"

identifierSpec :: ValuesSpec Identifier
identifierSpec = mkId <$> valuesSpec

-- | Resolve relative paths starting at the home directory rather than
-- the current directory of the client.
resolveConfigurationPath :: FilePath -> IO FilePath
resolveConfigurationPath path
  | isAbsolute path = return path
  | otherwise = do home <- getHomeDirectory
                   return (home </> path)

macroMapSpec :: ValuesSpec (Recognizer Macro)
macroMapSpec = fromCommands <$> listSpec macroValuesSpec

macroValuesSpec :: ValuesSpec (Text, Macro)
macroValuesSpec = sectionsSpec $
  do name     <- reqSection "name" ""
     spec     <- fromMaybe noMacroArguments
             <$> optSection' "arguments" "" macroArgumentsSpec
     commands <- reqSection' "commands" "" (listSpec macroCommandSpec)
     return (name, Macro spec commands)

macroArgumentsSpec :: ValuesSpec MacroSpec
macroArgumentsSpec = customSpec "macro arguments" valuesSpec parseMacroSpecs

macroCommandSpec :: ValuesSpec [ExpansionChunk]
macroCommandSpec = customSpec "macro command" valuesSpec parseExpansion

nickCompletionSpec :: ValuesSpec WordCompletionMode
nickCompletionSpec =
  defaultNickWordCompleteMode <$ atomSpec "default" <|>
  slackNickWordCompleteMode   <$ atomSpec "slack"
