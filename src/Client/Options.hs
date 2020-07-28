{-# Language CPP, TemplateHaskell, MultiWayIf #-}

#ifndef TOOL_VERSION_alex
#define TOOL_VERSION_alex "none"
#endif

#ifndef TOOL_VERSION_happy
#define TOOL_VERSION_happy "none"
#endif

{-|
Module      : Client.Options
Description : Processing of command-line options
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module process command-line options provided when launching the client.

-}
module Client.Options
  (
  -- * Command-line options
    Options(..)

  -- * Lenses
  , optConfigFile
  , optInitialNetworks
  , optNoConnect

  -- * Options loader
  , getOptions
  ) where

import           Config.Schema.Docs
import           Control.Lens
import           Data.Foldable
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Version
import           Development.GitRev (gitHash, gitDirty)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.Info
import           Paths_glirc (version)
import           Build_glirc (deps)

import           Client.Configuration

-- | Command-line options
data Options = Options
  { _optConfigFile      :: Maybe FilePath -- ^ configuration file path
  , _optInitialNetworks :: [Text]         -- ^ initial networks
  , _optNoConnect       :: Bool           -- ^ disable autoconnect
  , _optShowHelp        :: Bool           -- ^ show help message
  , _optShowVersion     :: Bool           -- ^ show version message
  , _optShowFullVersion :: Bool           -- ^ show version of ALL transitive dependencies
  , _optShowConfigFormat:: Bool           -- ^ show configuration file format
  }

makeLenses ''Options

-- | Default values for 'Options'
defaultOptions :: Options
defaultOptions = Options
  { _optConfigFile      = Nothing
  , _optInitialNetworks = []
  , _optShowHelp        = False
  , _optShowVersion     = False
  , _optShowFullVersion = False
  , _optNoConnect       = False
  , _optShowConfigFormat= False
  }

-- | Option descriptions
options :: [OptDescr (Options -> Options)]
options =
  [ Option "c" ["config"]  (ReqArg (set optConfigFile . Just) "PATH")
    "Configuration file path"
  , Option "!" ["noconnect"] (NoArg (set optNoConnect True))
    "Disable autoconnecting"
  , Option "h" ["help"]    (NoArg (set optShowHelp True))
    "Show help"
  , Option "" ["config-format"] (NoArg (set optShowConfigFormat True))
    "Show configuration file format"
  , Option "v" ["version"] (NoArg (set optShowVersion True))
    "Show version"
  , Option "" ["full-version"] (NoArg (set optShowFullVersion True))
    "Show version and versions of all linked Haskell libraries"
  ]

optOrder :: ArgOrder (Options -> Options)
optOrder = ReturnInOrder (\x -> optInitialNetworks <>~ [Text.pack x])

-- | Load command line options. This action will terminate early
-- in the case of the version flag, help flag, or an error.
getOptions :: IO Options
getOptions =
  do (flags, _, errors) <- getOpt optOrder options <$> getArgs

     let opts = foldl' (\acc f -> f acc) defaultOptions flags

         bullet x = "• " ++ x

         reportErrors =
           do hPutStrLn stderr "Errors processing command-line options:"
              traverse_ (hPutStr stderr) (map bullet errors)
              hPutStrLn stderr tryHelpTxt

     if | view optShowHelp         opts -> putStr helpTxt        >> exitSuccess
        | view optShowFullVersion  opts -> putStr fullVersionTxt >> exitSuccess
        | view optShowVersion      opts -> putStr versionTxt     >> exitSuccess
        | view optShowConfigFormat opts -> printConfigFormat     >> exitSuccess
        | null errors                   -> return opts
        | otherwise                     -> reportErrors          >> exitFailure

printConfigFormat :: IO ()
printConfigFormat =
  do path <- getNewConfigPath
     putStrLn ""
     putStrLn ("Default configuration file path: " ++ path)
     putStrLn ""
     print (generateDocs configurationSpec)

helpTxt :: String
helpTxt = usageInfo "glirc [FLAGS] INITIAL_NETWORKS..." options

tryHelpTxt :: String
tryHelpTxt =
  "Run 'glirc --help' to see a list of available command line options."

-- version information ---------------------------------------------

versionTxt :: String
versionTxt = unlines
  [ "glirc-" ++ showVersion version ++ gitHashTxt ++ gitDirtyTxt
  , "Copyright 2016-2020 Eric Mertens"
  ]

fullVersionTxt :: String
fullVersionTxt =
  versionTxt ++
  unlines
  (""
  :("OS          : " ++ os)
  :("Architecture: " ++ arch)
  :("Compiler    : " ++ compilerName ++ "-" ++ showVersion compilerVersion)
  :""
  :("ghc         : " ++ TOOL_VERSION_ghc)
  :("ghc-pkg     : " ++ TOOL_VERSION_ghc_pkg)
  :("alex        : " ++ TOOL_VERSION_alex)
  :("happy       : " ++ TOOL_VERSION_happy)
  :("hsc2hs      : " ++ TOOL_VERSION_hsc2hs)
  :""
  :"Transitive dependencies:"
  : [ name ++ "-" ++ intercalate "." (map show ver) | (name,ver) <- sort deps ]
  )

-- git version information ---------------------------------------------

-- | Returns @"-SOMEHASH"@ when in a git repository, @""@ otherwise.
gitHashTxt :: String
gitHashTxt
  | hashTxt == "UNKNOWN" = ""
  | otherwise            = '-':hashTxt
  where
    hashTxt = $gitHash

-- | Returns @"-dirty"@ when in a dirty git repository, @""@ otherwise.
gitDirtyTxt :: String
gitDirtyTxt
  | $gitDirty = "-dirty"
  | otherwise = ""
