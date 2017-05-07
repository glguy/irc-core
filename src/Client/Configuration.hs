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

  -- * Specification
  , configurationSpec
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
import           Data.Functor.Alt                    ((<!>))
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

     case loadValue configurationSpec rawcfg of
       Left loadError -> throwIO (ConfigurationMalformed (show loadError)) -- XXX show
       Right cfg -> return (cfg mbPath def)


configurationSpec :: ValueSpecs (Maybe FilePath -> ServerSettings -> Configuration)
configurationSpec = sectionsSpec "" $

  do ssDefUpdate <- fromMaybe id <$> optSection' "defaults" "" serverSpec
     ssUpdates   <- fromMaybe [] <$> optSection' "servers" "" (listSpec serverSpec)

     _configPalette <- fromMaybe defaultPalette
                    <$> optSection' "palette" "" paletteSpec

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

     return (\_configConfigPath def ->
             let _configDefaults = ssDefUpdate def
                 _configServers  = buildServerMap _configDefaults ssUpdates
             in Configuration{..})


nonnegativeSpec :: (Ord a, Num a) => ValueSpecs a
nonnegativeSpec = customSpec "non-negative" numSpec $ \x -> find (0 <=) [x]

filePathSpec :: ValueSpecs FilePath
filePathSpec = stringSpec


-- | Matches the 'yes' and 'no' atoms
yesOrNo :: ValueSpecs Bool
yesOrNo = True  <$ atomSpec "yes"
      <!> False <$ atomSpec "no"


paletteSpec :: ValueSpecs Palette
paletteSpec = sectionsSpec "palette" $
  do updates <- catMaybes <$> sequenceA
       [ fmap (set l) <$> optSection' lbl "" attrSpec | (lbl, Lens l) <- paletteMap ]
     nickColors <- optSection' "nick-colors" "" (nonemptyList attrSpec)
     return (let pal1 = foldl' (\acc f -> f acc) defaultPalette updates
             in case nickColors of
                  Nothing -> pal1
                  Just xs -> set palNicks (Vector.fromList (NonEmpty.toList xs)) pal1)

nonemptyList :: ValueSpecs a -> ValueSpecs (NonEmpty a)
nonemptyList s = customSpec "non-empty" (listSpec s) NonEmpty.nonEmpty


buildServerMap :: ServerSettings -> [ServerSettings -> ServerSettings] -> HashMap Text ServerSettings
buildServerMap def ups =
  HashMap.fromList [ (serverSettingName ss, ss) | up <- ups, let ss = up def ]
  where
    serverSettingName ss =
      fromMaybe (views ssHostName Text.pack ss)
                (view ssName ss)

serverSpec :: ValueSpecs (ServerSettings -> ServerSettings)
serverSpec = sectionsSpec "server-settings" $
  do updates <- catMaybes <$> sequenceA settings
     return (foldr (.) id updates)
  where
    req l s = set l <$> s

    opt l s = set l . Just <$> s
          <!> set l Nothing <$ atomSpec "clear"

    settings =
      [ optSection' "name" "The name used to identify this server in the client"
      $ opt ssName valuesSpec
      , optSection' "hostname" "Hostname of server"
      $ req ssHostName filePathSpec
      , optSection' "port" "Port number of server. Default 6667 without TLS or 6697 with TLS"
      $ opt ssPort numSpec
      , optSection' "nick" "Nicknames to connect with in order"
      $ req ssNicks nicksSpec
      , optSection' "password" "Server password"
      $ opt ssPassword valuesSpec
      , optSection' "username" "Second component of _!_@_ usermask"
      $ req ssUser valuesSpec
      , optSection' "realname" "\"GECOS\" name sent to server visible in /whois"
      $ req ssReal valuesSpec
      , optSection' "userinfo" "CTCP userinfo (currently unused)"
      $ req ssUserInfo valuesSpec
      , optSection' "sasl-username" "Username for SASL authentication to NickServ"
      $ opt ssSaslUsername valuesSpec
      , optSection' "sasl-password" "Password for SASL authentication to NickServ"
      $ opt ssSaslPassword valuesSpec
      , optSection' "sasl-ecdsa-key" "Path to ECDSA key for non-password SASL authentication"
      $ opt ssSaslEcdsaFile     filePathSpec
      , optSection' "tls" "Set to `yes` to enable secure connect. Set to `yes-insecure` to disable certificate checking."
      $ req ssTls useTlsSpec
      , optSection' "tls-client-cert" "Path to TLS client certificate"
      $ opt ssTlsClientCert     filePathSpec
      , optSection' "tls-client-key" "Path to TLS client key"
      $ opt ssTlsClientKey      filePathSpec
      , optSection' "tls-server-cert" "Path to CA certificate bundle"
      $ opt ssTlsServerCert     filePathSpec
      , optSection' "tls-ciphers" "OpenSSL cipher specification. Default to \"HIGH\""
      $ req ssTlsCiphers stringSpec
      , optSection' "socks-host" "Hostname of SOCKS5 proxy server"
      $ opt ssSocksHost stringSpec
      , optSection' "socks-port" "Port number of SOCKS5 proxy server"
      $ req ssSocksPort numSpec
      , optSection' "connect-cmds" "Command to be run upon successful connection to server"
      $ req ssConnectCmds $ listSpec macroCommandSpec
      , optSection' "chanserv-channels" "Channels with ChanServ permissions available"
      $ req ssChanservChannels  $ listSpec identifierSpec
      , optSection' "flood-penalty" "RFC 1459 rate limiting, seconds of penalty per message (default 2)"
      $ req ssFloodPenalty valuesSpec
      , optSection' "flood-threshold" "RFC 1459 rate limiting, seconds of allowed penalty accumulation (default 10)"
      $ req ssFloodThreshold valuesSpec
      , optSection' "message-hooks" "Special message hooks to enable: \"buffextras\" available"
      $ req ssMessageHooks valuesSpec
      , optSection' "reconnect-attempts" "Number of reconnection attempts on lost connection"
      $ req ssReconnectAttempts valuesSpec
      , optSection' "autoconnect" "Set to `yes` to automatically connect at client startup"
      $ req ssAutoconnect yesOrNo
      , optSection' "nick-completion" "Behavior for nickname completion with TAB"
      $ req ssNickCompletion nickCompletionSpec
      , optSection' "log-dir" "Path to log file directory for this server"
      $ opt ssLogDir            filePathSpec
      ]


nicksSpec :: ValueSpecs (NonEmpty Text)
nicksSpec = pure <$> valuesSpec
        <!> nonemptyList valuesSpec


useTlsSpec :: ValueSpecs UseTls
useTlsSpec =
      UseTls         <$ atomSpec "yes"
  <!> UseInsecureTls <$ atomSpec "yes-insecure"
  <!> UseInsecure    <$ atomSpec "no"

identifierSpec :: ValueSpecs Identifier
identifierSpec = mkId <$> valuesSpec

-- | Resolve relative paths starting at the home directory rather than
-- the current directory of the client.
resolveConfigurationPath :: FilePath -> IO FilePath
resolveConfigurationPath path
  | isAbsolute path = return path
  | otherwise = do home <- getHomeDirectory
                   return (home </> path)

macroMapSpec :: ValueSpecs (Recognizer Macro)
macroMapSpec = fromCommands <$> listSpec macroValueSpecs

macroValueSpecs :: ValueSpecs (Text, Macro)
macroValueSpecs = sectionsSpec "macro" $
  do name     <- reqSection "name" ""
     spec     <- fromMaybe noMacroArguments
             <$> optSection' "arguments" "" macroArgumentsSpec
     commands <- reqSection' "commands" "" (listSpec macroCommandSpec)
     return (name, Macro spec commands)

macroArgumentsSpec :: ValueSpecs MacroSpec
macroArgumentsSpec = customSpec "macro arguments" valuesSpec parseMacroSpecs

macroCommandSpec :: ValueSpecs [ExpansionChunk]
macroCommandSpec = customSpec "macro command" valuesSpec parseExpansion

nickCompletionSpec :: ValueSpecs WordCompletionMode
nickCompletionSpec =
      defaultNickWordCompleteMode <$ atomSpec "default"
  <!> slackNickWordCompleteMode   <$ atomSpec "slack"
