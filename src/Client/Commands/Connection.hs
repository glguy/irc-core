{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Connection
Description : Connection command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Connection (connectionCommands) where

import           Client.Commands.Arguments.Spec
import           Client.Commands.TabCompletion
import           Client.Commands.Types
import           Client.Commands.WordCompletion
import           Client.Configuration
import           Client.State
import           Client.State.Focus
import           Client.State.Network
import           Control.Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import           Irc.Commands (ircMode, ircQuit)

connectionCommands :: CommandSection
connectionCommands = CommandSection "Connection commands"

  [ Command
      (pure "connect")
      (simpleToken "network")
      "Connect to \^Bnetwork\^B by name.\n\
      \\n\
      \If no name is configured the hostname is the 'name'.\n"
    $ ClientCommand cmdConnect tabConnect

  , Command
      (pure "reconnect")
      (pure ())
      "Reconnect to the current network.\n"
    $ ClientCommand cmdReconnect noClientTab

  , Command
      (pure "disconnect")
      (pure ())
      "Immediately terminate the current network connection.\n\
      \\n\
      \See also: /quit /exit\n"
    $ NetworkCommand cmdDisconnect noNetworkTab

  , Command
      (pure "quit")
      (remainingArg "reason")
      "Gracefully disconnect the current network connection.\n\
      \\n\
      \\^Breason\^B: optional quit reason\n\
      \\n\
      \See also: /disconnect /exit\n"
    $ NetworkCommand cmdQuit simpleNetworkTab

  , Command
      (pure "cert")
      (pure ())
      "Show the TLS certificate for the current connection.\n"
    $ NetworkCommand cmdCert noNetworkTab

  , Command
      (pure "umode")
      (remainingArg "modes")
      "Apply a user-mode change.\n"
    $ NetworkCommand cmdUmode noNetworkTab
  ]

cmdUmode :: NetworkCommand String
cmdUmode cs st rest =
  do let args = Text.words (Text.pack rest)
     sendMsg cs (ircMode (view csNick cs) args)
     commandSuccess st

cmdConnect :: ClientCommand String
cmdConnect st networkStr =
  do -- abort any existing connection before connecting
     let network = Text.pack networkStr
     st' <- addConnection 0 Nothing Nothing network =<< abortNetwork network st
     commandSuccess
       $ changeFocus (NetworkFocus network) st'

cmdQuit :: NetworkCommand String
cmdQuit cs st rest =
  do let msg = Text.pack rest
     sendMsg cs (ircQuit msg)
     commandSuccess st

cmdDisconnect :: NetworkCommand ()
cmdDisconnect cs st _ =
  do st' <- abortNetwork (view csNetwork cs) st
     commandSuccess st'

-- | Reconnect to the currently focused network. It's possible
-- that we're not currently connected to a network, so
-- this is implemented as a client command.
cmdReconnect :: ClientCommand ()
cmdReconnect st _
  | Just network <- views clientFocus focusNetwork st =

      do let tm = preview (clientConnection network . csLastReceived . folded) st
         st' <- addConnection 0 tm Nothing network =<< abortNetwork network st
         commandSuccess
           $ changeFocus (NetworkFocus network) st'

  | otherwise = commandFailureMsg "command requires focused network" st

-- | @/connect@ tab completes known server names
tabConnect :: Bool -> ClientCommand String
tabConnect isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] networks isReversed st
  where
    networks = views clientConnections              HashMap.keys st
            ++ views (clientConfig . configServers) HashMap.keys st

cmdCert :: NetworkCommand ()
cmdCert _ st _ = commandSuccess (changeSubfocus FocusCert st)
