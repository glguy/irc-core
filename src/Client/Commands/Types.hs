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
import Client.State (ClientState, clientErrorMsg, clientConnection, clientFocus)
import Client.State.Focus (Focus)
import Client.State.Network (NetworkState, csNetwork)
import Control.Lens (set, view)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Irc.Identifier (Identifier)
import LensUtils (setStrict)

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

-- | Type of commands that operate on a window
type WindowCommand a = Focus -> ClientCommand a

-- | Type of commands that require an active network to be focused
type NetworkCommand a = NetworkState {- ^ current network -} -> ClientCommand a

-- | Type of commands that require an active network to be focused and maybe a chat window.
type MaybeChatCommand a = Maybe Identifier {- ^ focused channel -} -> NetworkCommand a

-- | Type of commands that require an active channel to be focused
type ChannelCommand a = Identifier {- ^ focused channel -} -> NetworkCommand a


-- | Pair of implementations for executing a command and tab completing one.
-- The tab-completion logic is extended with a Bool
-- indicating that tab completion should be reversed
data CommandImpl a
  -- | no requirements
  = ClientCommand    (ClientCommand  a)   (Bool -> ClientCommand String)
  -- | operates on a window
  | WindowCommand    (WindowCommand a)    (Bool -> WindowCommand String)
  -- | requires an active network
  | NetworkCommand   (NetworkCommand a)   (Bool -> NetworkCommand String)
  -- | requires an active network and maybe a chat window
  | MaybeChatCommand (MaybeChatCommand a) (Bool -> MaybeChatCommand String)
  -- | requires an active chat window
  | ChatCommand      (ChannelCommand a)   (Bool -> ChannelCommand String)
  -- | requires an active channel window
  | ChannelCommand   (ChannelCommand a)   (Bool -> ChannelCommand String)

-- | Data available to the arguments parser at the time of parsing.
data ArgsContext = ArgsContext
  { argsContextSt    :: ClientState 
  , argsContextFocus :: Focus
  }

makeArgsContext :: ClientState -> ArgsContext
makeArgsContext st = ArgsContext {argsContextSt=st, argsContextFocus=view clientFocus st}

-- | A command is a list of aliases, an argument specification, implementation,
-- and documentation. The arguments and implementation must match so that
-- the parsed arguments will match what the implementation expects.
data Command = forall a. Command
  { -- | Names of this command, first in the list is the "primary" name
    cmdNames          :: NonEmpty Text
  -- | Specification of the arguments of the command
  , cmdArgumentSpec   :: Args ArgsContext a
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

-- | Consider the text entry successful, and resume the client with
-- a particular network updated.
commandSuccessUpdateCS :: NetworkState -> ClientState -> IO CommandResult
commandSuccessUpdateCS cs st =
  do let network = view csNetwork cs
     commandSuccess
       $ setStrict (clientConnection network) cs st

-- | Consider the text entry a failure and resume the client
commandFailure :: Monad m => ClientState -> m CommandResult
commandFailure = return . CommandFailure

-- | Command failure with an error message printed to client window
commandFailureMsg :: Text -> ClientState -> IO CommandResult
commandFailureMsg e st =
  return $! CommandFailure $! set clientErrorMsg (Just e) st
