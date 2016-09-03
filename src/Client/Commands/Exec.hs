{-# LANGUAGE TemplateHaskell, BangPatterns, OverloadedStrings #-}

{-|
Module      : Client.Commands
Description : Implementation of slash commands
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module renders the lines used in the channel mask list. A mask list
can show channel bans, quiets, invites, and exceptions.
-}

module Client.Commands.Exec where

import           Control.Exception
import           Control.Lens
import           System.Console.GetOpt
import           System.Process

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
  { _execOutputNetwork :: Maybe String -- ^ output network
  , _execOutputChannel :: Maybe String -- ^ output channel
  , _execCommand       :: String       -- ^ command filename
  , _execStdIn         :: String       -- ^ stdin source
  , _execArguments     :: [String]     -- ^ command arguments
  }
  deriving (Read,Show)

makeLenses ''ExecCmd

-- | Default values for @/exec@ to be overridden by flags.
emptyExecCmd :: ExecCmd
emptyExecCmd = ExecCmd
  { _execOutputNetwork = Nothing
  , _execOutputChannel = Nothing
  , _execCommand       = error "no default command"
  , _execStdIn         = ""
  , _execArguments     = []
  }

options :: [OptDescr (ExecCmd -> ExecCmd)]
options =
  [ Option "n" ["network"]
        (ReqArg (set execOutputNetwork . Just) "NETWORK")
        "Set network target"
  , Option "c" ["channel"]
        (ReqArg (set execOutputChannel . Just) "CHANNEL")
        "Set channel target"
  , Option "i" ["input"]
        (ReqArg (set execStdIn) "INPUT")
        "Use string as stdin"
  ]

-- TODO: support quoted strings
parseExecCmd :: String -> Either [String] ExecCmd
parseExecCmd str =
  case getOpt RequireOrder options (words str) of
    (_, [] , errs) -> Left ("No command specified":errs)
    (fs, cmd:args, []) -> Right
                        $ foldl (\x f -> f x) ?? fs
                        $ set execCommand cmd
                        $ set execArguments args
                        $ emptyExecCmd
    (_,_, errs) -> Left errs

runExecCmd :: ExecCmd -> IO (Either [String] [String])
runExecCmd e =
  do res <- try (readProcess (view execCommand e)
                             (view execArguments e)
                             (view execStdIn e))
     return $ case res of
       Left er -> Left [show (er :: IOError)]
       Right x -> Right (lines x)
