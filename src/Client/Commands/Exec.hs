{-# LANGUAGE DeriveFunctor, TemplateHaskell, BangPatterns, OverloadedStrings #-}

{-|
Module      : Client.Commands
Description : Implementation of slash commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel mask list. A mask list
can show channel bans, quiets, invites, and exceptions.
-}

module Client.Commands.Exec
  ( -- * Exec command configuration
    ExecCmd(..)
  , Target(..)

  -- * Lenses
  , execOutputNetwork
  , execOutputChannel

  -- * Operations
  , parseExecCmd
  , runExecCmd
  ) where

import Control.Exception (Exception(displayException))
import Control.Lens (view, (??), set, makeLenses)
import Data.ByteString.Lazy qualified as L
import Data.List (unfoldr)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Console.GetOpt (getOpt, ArgDescr(ReqArg, OptArg, NoArg), ArgOrder(RequireOrder), OptDescr(..))
import System.Process.Typed (byteStringInput, proc, readProcessStdout, setStdin, ExitCode (ExitFailure))

-- | Settings for @/exec@ command.
--
-- When no network or channel are specified the output is sent to the client
-- window.
--
-- When only a network is specified the output is sent as raw IRC commands to
-- that network.
--
-- When only a channel is specified the output is sent as messages on the
-- current network to the given channel.
--
-- When the network and channel are specified the output is sent as messages
-- to the given channel on the given network.
data ExecCmd = ExecCmd
  { _execOutputNetwork :: Target String -- ^ output network
  , _execOutputChannel :: Target String -- ^ output channel
  , _execCommand       :: String        -- ^ command filename
  , _execStdIn         :: String        -- ^ stdin source
  , _execArguments     :: [String]      -- ^ command arguments
  , _execIgnoreError   :: Bool          -- ^ ignore the process exit code
  }
  deriving (Read,Show)

data Target a = Unspecified | Current | Specified a
  deriving (Show, Read, Eq, Ord, Functor)

makeLenses ''ExecCmd

-- | Default values for @/exec@ to be overridden by flags.
emptyExecCmd :: ExecCmd
emptyExecCmd = ExecCmd
  { _execOutputNetwork = Unspecified
  , _execOutputChannel = Unspecified
  , _execCommand       = error "no default command"
  , _execStdIn         = ""
  , _execArguments     = []
  , _execIgnoreError   = False
  }

options :: [OptDescr (ExecCmd -> ExecCmd)]
options =
  let specified = maybe Current Specified in
  [ Option "n" ["network"]
        (OptArg (set execOutputNetwork . specified) "NETWORK")
        "Set network target"
  , Option "c" ["channel"]
        (OptArg (set execOutputChannel . specified) "CHANNEL")
        "Set channel target"
  , Option "i" ["input"]
        (ReqArg (set execStdIn) "INPUT")
        "Use string as stdin"
  , Option "e" ["error"]
        (NoArg (set execIgnoreError True))
        "Ignore process error codes"
  ]

-- | Parse the arguments to @/exec@ looking for various flags
-- and the command and its arguments.
parseExecCmd ::
  String                  {- ^ exec arguments          -} ->
  Either [String] ExecCmd {- ^ error or parsed command -}
parseExecCmd str =
  case getOpt RequireOrder options (powerWords str) of
    (_, [] , errs) -> Left ("No command specified":errs)
    (fs, cmd:args, []) -> Right
                        $ foldl (\x f -> f x) ?? fs
                        $ set execCommand cmd
                        $ set execArguments args
                        $ emptyExecCmd
    (_,_, errs) -> Left errs

-- | Execute the requested command synchronously and return
-- the output.
runExecCmd ::
  ExecCmd                       {- ^ exec configuration          -} ->
  IO (Either [String] [String]) {- ^ error lines or output lines -}
runExecCmd cmd =
 do res <-
      readProcessStdout
        (setStdin (byteStringInput (L.fromStrict (Text.encodeUtf8 (Text.pack (view execStdIn cmd)))))
        (proc (view execCommand   cmd) (view execArguments cmd)))
    return $! case res of
      (ExitFailure code, _) | not (view execIgnoreError cmd) ->
        Left ["Process failed with exit code " ++ show code]
      (_, out) ->
        case Text.decodeUtf8' (L.toStrict out) of
          Right str -> Right (lines (Text.unpack str))
          Left e -> Left [displayException e]

-- | Power words is similar to 'words' except that when it encounters
-- a word formatted as a Haskell 'String' literal it parses it as
-- such. Only space is used as a delimiter.
powerWords :: String -> [String]
powerWords = unfoldr (splitWord . dropWhile isSp)
  where
    isSp x = x == ' '

    splitWord xs
      | null xs         = Nothing
      | [x] <- reads xs = Just x
      | otherwise       = Just (break isSp xs)
