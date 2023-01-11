{-# Language ExistentialQuantification #-}
{-|
Module      : Client.Commands.Types
Description : Types used to implement client commands
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Types where

import Client.Commands.Arguments.Spec (Args)
import Client.State (ClientState, clientErrorMsg)
import Client.State.Network (NetworkState)
import Control.Lens (set)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Irc.Identifier (Identifier)

-- | Possible results of running a command
data CommandResult
  -- | Continue running the client, consume input if command was from input
  = CommandSuccess ClientState
  -- | Continue running the client, report an error
  | CommandFailure ClientState
  -- | Client should close
  | CommandQuit ClientState


-- | Type of commands that always work
type ClientCommand a = ClientState -> a {- ^ arguments -} -> IO CommandResult

-- | Type of commands that require an active network to be focused
type NetworkCommand a = NetworkState {- ^ current network -} -> ClientCommand a

-- | Type of commands that require an active channel to be focused
type ChannelCommand a = Identifier {- ^ focused channel -} -> NetworkCommand a


-- | Pair of implementations for executing a command and tab completing one.
-- The tab-completion logic is extended with a Bool
-- indicating that tab completion should be reversed
data CommandImpl a
  -- | no requirements
  = ClientCommand  (ClientCommand  a) (Bool -> ClientCommand  String)
  -- | requires an active network
  | NetworkCommand (NetworkCommand a) (Bool -> NetworkCommand String)
  -- | requires an active chat window
  | ChatCommand    (ChannelCommand a) (Bool -> ChannelCommand String)
  -- | requires an active channel window
  | ChannelCommand (ChannelCommand a) (Bool -> ChannelCommand String)


-- | A command is a list of aliases, an argument specification, implementation,
-- and documentation. The arguments and implementation must match so that
-- the parsed arguments will match what the implementation expects.
data Command = forall a. Command
  { -- | Names of this command, first in the list is the "primary" name
    cmdNames          :: NonEmpty Text
  -- | Specification of the arguments of the command
  , cmdArgumentSpec   :: Args ClientState a
  -- | Multi-line IRC-formatted documentation text used for @/help@
  , cmdDocumentation  :: Text
  -- | Implementation of the command for both execution and tab completion
  , cmdImplementation :: CommandImpl a
  }

-- | A command section is a logical grouping of commands. This allows for
-- showing more structure in the help menu system.
data CommandSection = CommandSection
  { cmdSectionName :: Text
  , cmdSectionCmds :: [Command]
  }

-- | Consider the text entry successful and resume the client
commandSuccess :: Monad m => ClientState -> m CommandResult
commandSuccess = return . CommandSuccess

-- | Consider the text entry a failure and resume the client
commandFailure :: Monad m => ClientState -> m CommandResult
commandFailure = return . CommandFailure

-- | Command failure with an error message printed to client window
commandFailureMsg :: Text -> ClientState -> IO CommandResult
commandFailureMsg e st =
  return $! CommandFailure $! set clientErrorMsg (Just e) st
