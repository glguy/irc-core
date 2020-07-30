{-|
Module      : Client.Commands.TabCompletion
Description : Common tab-completion logic
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.TabCompletion where

import           Client.Commands.Types
import           Client.Commands.WordCompletion
import           Client.Message
import           Client.State
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window
import           Client.State.Channel
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import           Irc.Identifier
import           Irc.UserInfo

-- | Provides no tab completion for client commands
noClientTab :: Bool -> ClientCommand String
noClientTab _ st _ = commandFailure st

-- | Provides no tab completion for network commands
noNetworkTab :: Bool -> NetworkCommand String
noNetworkTab _ _ st _ = commandFailure st

-- | Provides no tab completion for channel commands
noChannelTab :: Bool -> ChannelCommand String
noChannelTab _ _ _ st _ = commandFailure st

-- | Provides nickname based tab completion for client commands
simpleClientTab :: Bool -> ClientCommand String
simpleClientTab isReversed st _ =
  nickTabCompletion isReversed st

-- | Provides nickname based tab completion for network commands
simpleNetworkTab :: Bool -> NetworkCommand String
simpleNetworkTab isReversed _ st _ =
  nickTabCompletion isReversed st

-- | Provides nickname based tab completion for channel commands
simpleChannelTab :: Bool -> ChannelCommand String
simpleChannelTab isReversed _ _ st _ =
  nickTabCompletion isReversed st

simpleTabCompletion ::
  Prefix a =>
  WordCompletionMode {- ^ word completion mode -} ->
  [a]                {- ^ hints                -} ->
  [a]                {- ^ all completions      -} ->
  Bool               {- ^ reversed order       -} ->
  ClientState        {- ^ client state         -} ->
  IO CommandResult
simpleTabCompletion mode hints completions isReversed st =
  case traverseOf clientTextBox tryCompletion st of
    Nothing  -> commandFailure st
    Just st' -> commandSuccess st'
  where
    tryCompletion = wordComplete mode isReversed hints completions

-- | Complete the nickname at the current cursor position using the
-- userlist for the currently focused channel (if any)
nickTabCompletion :: Bool {- ^ reversed -} -> ClientState -> IO CommandResult
nickTabCompletion isReversed st =
  simpleTabCompletion mode hint completions isReversed st
  where
    hint          = activeNicks st
    completions   = currentCompletionList st
    mode          = currentNickCompletionMode st

activeNicks ::
  ClientState ->
  [Identifier]
activeNicks st =
  case view clientFocus st of
    focus@(ChannelFocus network channel) ->
      toListOf
        ( clientWindows    . ix focus
        . winMessages      . each
        . wlSummary        . folding summaryActor
        . filtered isActive
        . filtered isNotSelf ) st
      where
        isActive n = HashMap.member n userMap
        self = preview ( clientConnection network . csNick ) st
        isNotSelf n = case self of
                        Nothing -> True
                        Just s -> n /= s
        userMap = view ( clientConnection network
                       . csChannels . ix channel
                       . chanUsers) st

    _ -> []

  where
    -- Returns the 'Identifier' of the nickname responsible for
    -- the window line when that action was significant enough to
    -- be considered a hint for tab completion.
    summaryActor :: IrcSummary -> Maybe Identifier
    summaryActor (ChatSummary who) = Just $! userNick who
    summaryActor _                 = Nothing

