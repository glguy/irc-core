{-# LANGUAGE OverloadedStrings #-}
module ConnectCmds (connectCmds) where

import Control.Lens
import Data.Foldable (for_)
import Data.Text.Encoding
import Data.Monoid ((<>))

import Irc.Message
import Irc.Format
import ClientState
import ServerSettings

connectCmds :: EventHandler
connectCmds = EventHandler
  { _evName = "connect commands"
  , _evOnEvent = handler
  }

handler :: Identifier -> IrcMessage -> ClientState -> IO ClientState

handler ident mesg st
  | ident == ""
  , views mesgSender userNick mesg == "Welcome" =
      do let cmds = view (clientServer0 . ccServerSettings . ssConnectCmds) st
         for_ cmds $ \cmd ->
           clientSend (encodeUtf8 cmd<>"\r\n") st
         return st

  | otherwise = return (reschedule st)

-- Reschedule the autojoin handler for the next message
reschedule :: ClientState -> ClientState
reschedule = over clientAutomation (cons connectCmds)
