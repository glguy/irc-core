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
import           Client.Configuration.Macros (macroMapSpec)
import           Client.Configuration.ServerSettings
import           Client.Image.Palette
import           Config
import           Config.Schema
import           Control.Applicative
import           Control.Exception
import           Control.Lens                        hiding (List)
import           Data.Foldable                       (toList, find, foldl')
import           Data.Functor.Alt                    ((<!>))
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.HashSet                        (HashSet)
import qualified Data.HashSet                        as HashSet
import           Data.List.NonEmpty                  (NonEmpty)
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.Maybe
import           Data.Monoid                         (Endo(..), (<>))
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
       Left es -> throwIO
                $ ConfigurationMalformed
                $ Text.unpack
                $ Text.unlines
                $ map explainLoadError
                $ toList es
       Right cfg -> return (cfg mbPath def)


explainLoadError :: LoadError -> Text
explainLoadError (LoadError pos path problem) =
  Text.concat [ positionText, " at ", pathText, " - ", problemText]

  where
    positionText =
     Text.unwords ["line"  , Text.pack (show (posLine   pos)),
                   "column", Text.pack (show (posColumn pos))]

    pathText
      | null path = "top-level"
      | otherwise = Text.intercalate ":" path

    problemText =
      case problem of
        UnusedSection  s -> "unknown section `"          <> s <> "`"
        MissingSection s -> "missing required section `" <> s <> "`"
        SpecMismatch   t -> "expected "                  <> t


configurationSpec :: ValueSpecs (Maybe FilePath -> ServerSettings -> Configuration)
configurationSpec = sectionsSpec "" $

  do let sec' def name info spec = fromMaybe def <$> optSection' name info spec
         identifierSetSpec       = HashSet.fromList <$> listSpec identifierSpec

     ssDefUpdate            <- sec' id     "defaults"         "" serverSpec
     ssUpdates              <- sec' []     "servers"          "" (listSpec serverSpec)
     _configPalette         <- sec' defaultPalette "palette"  "" paletteSpec
     _configWindowNames     <- sec' defaultWindowNames "window-names" "" valuesSpec
     _configMacros          <- sec' mempty "macros"           "" macroMapSpec
     _configExtensions      <- sec' []     "extensions"       "" (listSpec stringSpec)
     _configUrlOpener       <- optSection' "url-opener"       "" stringSpec
     _configExtraHighlights <- sec' mempty "extra-highlights" "" identifierSetSpec
     _configNickPadding     <- optSection' "nick-padding"     "" nonnegativeSpec
     _configIndentWrapped   <- optSection' "indent-wrapped-lines" "" nonnegativeSpec
     _configIgnores         <- sec' mempty "ignores"          "" identifierSetSpec
     _configActivityBar     <- sec' False  "activity-bar"     "" yesOrNoSpec
     _configBellOnMention   <- sec' False  "bell-on-mention"  "" yesOrNoSpec

     return (\_configConfigPath def ->
             let _configDefaults = ssDefUpdate def
                 _configServers  = buildServerMap _configDefaults ssUpdates
             in Configuration{..})


nonnegativeSpec :: (Ord a, Num a) => ValueSpecs a
nonnegativeSpec = customSpec "non-negative" numSpec $ \x -> find (0 <=) [x]


paletteSpec :: ValueSpecs Palette
paletteSpec = sectionsSpec "palette" $
  (ala Endo (foldMap . foldMap) ?? defaultPalette) <$> sequenceA fields

  where
    nickColorsSpec = set palNicks . Vector.fromList . NonEmpty.toList <$> nonemptySpec attrSpec

    fields :: [SectionSpecs (Maybe (Palette -> Palette))]
    fields = optSection' "nick-colors" "" nickColorsSpec
           : [ optSection' lbl "" (set l <$> attrSpec) | (lbl, Lens l) <- paletteMap ]


buildServerMap :: ServerSettings -> [ServerSettings -> ServerSettings] -> HashMap Text ServerSettings
buildServerMap def ups =
  HashMap.fromList [ (serverSettingName ss, ss) | up <- ups, let ss = up def ]
  where
    serverSettingName ss =
      fromMaybe (views ssHostName Text.pack ss)
                (view ssName ss)


-- | Resolve relative paths starting at the home directory rather than
-- the current directory of the client.
resolveConfigurationPath :: FilePath -> IO FilePath
resolveConfigurationPath path
  | isAbsolute path = return path
  | otherwise = do home <- getHomeDirectory
                   return (home </> path)
