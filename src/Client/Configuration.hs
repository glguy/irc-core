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
  , LayoutMode(..)
  , PaddingMode(..)

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
  , configBellOnMention
  , configHideMeta
  , configKeyMap
  , configLayout
  , configJumpModifier

  -- * Loading configuration
  , loadConfiguration

  -- * Resolving paths
  , resolveConfigurationPath
  , getNewConfigPath

  -- * Specification
  , configurationSpec
  ) where

import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Configuration.Colors
import           Client.Configuration.Macros (macroMapSpec)
import           Client.Configuration.ServerSettings
import           Client.EventLoop.Actions
import           Client.Image.Palette
import           Config
import           Config.Schema
import           Control.Exception
import           Control.Lens                        hiding (List)
import           Data.Foldable                       (toList, find)
import           Data.Functor.Alt                    ((<!>))
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.HashSet                        (HashSet)
import qualified Data.HashSet                        as HashSet
import qualified Data.List.NonEmpty                  as NonEmpty
import           Data.Maybe
import           Data.Monoid                         (Endo(..), (<>))
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.IO                        as Text
import qualified Data.Vector                         as Vector
import           Graphics.Vty.Input.Events (Modifier(..), Key(..))
import           Irc.Identifier                      (Identifier)
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
  , _configNickPadding     :: PaddingMode -- ^ Padding of nicks in messages
  , _configConfigPath      :: Maybe FilePath
        -- ^ manually specified configuration path, used for reloading
  , _configMacros          :: Recognizer Macro -- ^ command macros
  , _configExtensions      :: [FilePath] -- ^ paths to shared library
  , _configUrlOpener       :: Maybe FilePath -- ^ paths to url opening executable
  , _configIgnores         :: HashSet Identifier -- ^ initial ignore list
  , _configActivityBar     :: Bool -- ^ initially visibility of the activity bar
  , _configBellOnMention   :: Bool -- ^ notify terminal on mention
  , _configHideMeta        :: Bool -- ^ default setting for hidemeta on new windows
  , _configKeyMap          :: KeyMap -- ^ keyboard bindings
  , _configLayout          :: LayoutMode -- ^ Default layout on startup
  , _configJumpModifier    :: [Modifier] -- ^ Modifier used for jumping windows
  }
  deriving Show

-- | Setting for how to pad the message prefix.
data PaddingMode
  = LeftPadding  !Int -- ^ Whitespace add to the left side of chat prefix
  | RightPadding !Int -- ^ Whitespace add to the right side of chat prefix
  | NoPadding         -- ^ No whitespace added
  deriving (Show)

data LayoutMode
  -- | Vertically stack all windows in a single column
  = OneColumn
  -- | Vertically stack extra windows in a second column
  | TwoColumn
  deriving Show

makeLenses ''Configuration

-- | Failure cases when loading a configuration file.
data ConfigurationFailure

  -- | Error message from reading configuration file
  = ConfigurationReadFailed String

  -- | Error message from parser or lexer
  | ConfigurationParseFailed FilePath String

  -- | Error message from loading parsed configuration
  | ConfigurationMalformed FilePath String
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
  (IOError -> IO (FilePath, Text)) {- ^ error handler for not found case -} ->
  IO (FilePath, Text)
readFileCatchNotFound path onNotFound =
  do res <- try (Text.readFile path)
     case res of
       Left e | isDoesNotExistError e -> onNotFound e
              | otherwise -> throwIO (ConfigurationReadFailed (show e))
       Right txt -> return (path, txt)

-- | Either read a configuration file from one of the default
-- locations, in which case no configuration found is equivalent
-- to an empty configuration, or from the specified file where
-- no configuration found is an error.
readConfigurationFile ::
  Maybe FilePath {- ^ just file or use default search paths -} ->
  IO (FilePath, Text)
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
                return ("", emptyConfigFile)


-- | Load the configuration file defaulting to @~/.glirc/config@.
loadConfiguration ::
  Maybe FilePath {- ^ path to configuration file -} ->
  IO (Either ConfigurationFailure Configuration)
loadConfiguration mbPath = try $
  do (path,txt) <- readConfigurationFile mbPath
     def  <- loadDefaultServerSettings

     rawcfg <-
       case parse txt of
         Left e -> throwIO (ConfigurationParseFailed path (displayException e))
         Right rawcfg -> return rawcfg

     case loadValue configurationSpec rawcfg of
       Left es -> throwIO
                $ ConfigurationMalformed path
                $ Text.unpack
                $ Text.unlines
                $ map explainLoadError (toList es)
       Right cfg -> return (cfg mbPath def)


explainLoadError :: LoadError Position -> Text
explainLoadError (LoadError pos path problem) =
  Text.concat [ positionText, " at ", pathText, ": ", problemText]

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

  do let sec' def name spec info = fromMaybe def <$> optSection' name spec info
         identifierSetSpec       = HashSet.fromList <$> listSpec identifierSpec

     ssDefUpdate            <- sec' id "defaults" serverSpec
                               "Default values for use across all server configurations"
     ssUpdates              <- sec' [] "servers" (listSpec serverSpec)
                               "Configuration parameters for IRC servers"
     _configPalette         <- sec' defaultPalette "palette" paletteSpec
                               "Customize the client color choices"
     _configWindowNames     <- sec' defaultWindowNames "window-names" valuesSpec
                               "Window names to use for quick jumping with jump-modifier key"
     _configJumpModifier    <- sec' [MMeta] "jump-modifier" modifierSpec
                               "Modifier used to jump to a window by name. Defaults to `meta`."
     _configMacros          <- sec' mempty "macros" macroMapSpec
                               "Programmable macro commands"
     _configExtensions      <- sec' [] "extensions" (listSpec stringSpec)
                               "Filenames of extension libraries to load at startup"
     _configUrlOpener       <- optSection' "url-opener" stringSpec
                               "External command used by /url command"
     _configExtraHighlights <- sec' mempty "extra-highlights" identifierSetSpec
                               "Extra words to highlight in chat messages"
     _configNickPadding     <- sec' NoPadding "nick-padding" nickPaddingSpec
                               "Amount of space to reserve for nicknames in chat messages"
     _configIgnores         <- sec' mempty "ignores" identifierSetSpec
                               "Set of nicknames to ignore on startup"
     _configActivityBar     <- sec' False  "activity-bar" yesOrNoSpec
                               "Show channel names and message counts for activity on\
                               \ unfocused channels."
     _configBellOnMention   <- sec' False  "bell-on-mention" yesOrNoSpec
                               "Emit bell character to terminal on mention"
     _configHideMeta        <- sec' False  "hide-metadata" yesOrNoSpec
                               "Initial setting for hiding metadata on new windows"
     bindings               <- sec' [] "key-bindings" (listSpec keyBindingSpec)
                               "Extra key bindings"
     _configLayout          <- sec' OneColumn "layout" layoutSpec
                               "Initial setting for window layout"
     return (\_configConfigPath def ->
             let _configDefaults = ssDefUpdate def
                 _configServers  = buildServerMap _configDefaults ssUpdates
                 _configKeyMap   = foldl (\acc f -> f acc) initialKeyMap bindings
             in Configuration{..})

-- | The default nick padding side if padding is going to be used
defaultPaddingSide :: Int -> PaddingMode
defaultPaddingSide = RightPadding

-- | Either full or abbreviated nick-padding configuration
--
-- > nick-padding: 10
--
-- > nick-padding:
-- >   side: right
-- >   width: 16
nickPaddingSpec :: ValueSpecs PaddingMode
nickPaddingSpec = defaultPaddingSide <$> nonnegativeSpec <!> fullNickPaddingSpec

-- | Full nick padding specification:
--
-- > nick-padding:
-- >   side: left
-- >   width: 15
fullNickPaddingSpec :: ValueSpecs PaddingMode
fullNickPaddingSpec = sectionsSpec "nick-padding" (sideSec <*> amtSec)
  where
    sideSpec = LeftPadding  <$ atomSpec "left" <!>
               RightPadding <$ atomSpec "right"

    sideSec = fromMaybe defaultPaddingSide
          <$> optSection' "side" sideSpec "Side to pad (default `right`)"

    amtSec  = reqSection' "width" nonnegativeSpec "Field width"


modifierSpec :: ValueSpecs [Modifier]
modifierSpec = toList <$> oneOrNonemptySpec modifier1Spec
  where
    modifier1Spec = namedSpec "modifier"
                  $ MMeta <$ atomSpec "meta"
                <!> MAlt  <$ atomSpec "alt"
                <!> MCtrl <$ atomSpec "ctrl"

layoutSpec :: ValueSpecs LayoutMode
layoutSpec = OneColumn <$ atomSpec "one-column"
         <!> TwoColumn <$ atomSpec "two-column"

keyBindingSpec :: ValueSpecs (KeyMap -> KeyMap)
keyBindingSpec = actBindingSpec <!> cmdBindingSpec <!> unbindingSpec

actBindingSpec :: ValueSpecs (KeyMap -> KeyMap)
actBindingSpec = sectionsSpec "action-binding" $
  do (m,k) <- reqSection' "bind" keySpec
              "Key to be bound (e.g. a, C-b, M-c C-M-d)"
     a     <- reqSection "action"
              "Action name (see `/keymap`)"
     return (addKeyBinding m k a)

cmdBindingSpec :: ValueSpecs (KeyMap -> KeyMap)
cmdBindingSpec = sectionsSpec "command-binding" $
  do (m,k) <- reqSection' "bind" keySpec
              "Key to be bound (e.g. a, C-b, M-c C-M-d)"
     cmd   <- reqSection "command"
              "Client command to execute (exclude leading `/`)"
     return (addKeyBinding m k (ActCommand cmd))

unbindingSpec :: ValueSpecs (KeyMap -> KeyMap)
unbindingSpec = sectionsSpec "remove-binding" $
  do (m,k) <- reqSection' "unbind" keySpec
              "Key to be unbound (e.g. a, C-b, M-c C-M-d)"
     return (removeKeyBinding m k)


-- | Custom configuration specification for emacs-style key descriptions
keySpec :: ValueSpecs ([Modifier], Key)
keySpec = customSpec "emacs-key" stringSpec parseKey


nonnegativeSpec :: (Ord a, Num a) => ValueSpecs a
nonnegativeSpec = customSpec "non-negative" numSpec $ \x -> find (0 <=) [x]


paletteSpec :: ValueSpecs Palette
paletteSpec = sectionsSpec "palette" $
  (ala Endo (foldMap . foldMap) ?? defaultPalette) <$> sequenceA fields

  where
    nickColorsSpec = set palNicks . Vector.fromList . NonEmpty.toList <$> nonemptySpec attrSpec

    fields :: [SectionSpecs (Maybe (Palette -> Palette))]
    fields = optSection' "nick-colors" nickColorsSpec
             "Colors used to highlight nicknames"
           : [ optSection' lbl (set l <$> attrSpec) "" | (lbl, Lens l) <- paletteMap ]


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
