{-# Language TemplateHaskell #-}
{-|
Module      : Client.CommandArguments
Description : Processing of command-line arguments
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module process command-line arguments provided when
launching the client.

-}
module Client.CommandArguments
  (
  -- * Command-line argument type
    CommandArguments(..)
  , cmdArgConfigFile
  , cmdArgInitialNetworks

  -- * Argument loader
  , getCommandArguments
  ) where

import           Control.Lens
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Version
import           Development.GitRev (gitHash, gitDirty)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Paths_glirc (version)

-- | Command-line arguments
data CommandArguments = CommandArguments
  { _cmdArgConfigFile      :: Maybe FilePath -- ^ configuration file path
  , _cmdArgInitialNetworks :: [Text]         -- ^ initial networks
  , _cmdArgShowHelp        :: Bool           -- ^ show help message
  , _cmdArgShowVersion     :: Bool           -- ^ show version message
  }

makeLenses ''CommandArguments

-- | Default values for arguments
defaultCommandArguments :: CommandArguments
defaultCommandArguments = CommandArguments
  { _cmdArgConfigFile      = Nothing
  , _cmdArgInitialNetworks = []
  , _cmdArgShowHelp        = False
  , _cmdArgShowVersion     = False
  }

-- | Option descriptions
options :: [OptDescr (CommandArguments -> CommandArguments)]
options =
  [ Option "c" ["config"]  (ReqArg (set cmdArgConfigFile . Just) "PATH")
    "Configuration file path"
  , Option "h" ["help"]    (NoArg (set cmdArgShowHelp True))
    "Show help"
  , Option "v" ["version"] (NoArg (set cmdArgShowVersion True))
    "Show version"
  ]

-- | Load command line arguments. This action will terminate early
-- in the case of the version flag, help flag, or an error.
getCommandArguments :: IO CommandArguments
getCommandArguments =
  do args <- getArgs
     case getOpt Permute options args of
       (flags, networks, [])
         | view cmdArgShowHelp    cmdArgs -> putStr helpTxt    >> exitSuccess
         | view cmdArgShowVersion cmdArgs -> putStr versionTxt >> exitSuccess
         | otherwise                      -> return cmdArgs
         where
           cmdArgs = assembleCommandArguments flags networks
       (_, _, errors) ->
         do traverse_ (hPutStr stderr) errors
            hPutStrLn stderr "Run 'glirc2 --help' to see a list of available command line options."
            exitFailure

assembleCommandArguments :: [CommandArguments -> CommandArguments] -> [String] -> CommandArguments
assembleCommandArguments flags networks =
  let flagArgs = foldl' (\acc f -> f acc) defaultCommandArguments flags
  in flagArgs { _cmdArgInitialNetworks = Text.pack <$> networks }

helpTxt :: String
helpTxt = usageInfo "glirc2 [FLAGS] INITIAL_NETWORKS..." options

versionTxt :: String
versionTxt = unlines
  [ "glirc-" ++ showVersion version ++ gitHashTxt ++ gitDirtyTxt
  , "Copyright 2016 Eric Mertens"
  ]

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
