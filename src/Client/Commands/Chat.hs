{-# Language BangPatterns, OverloadedStrings #-}
{-|
Module      : Client.Commands.Chat
Description : Common user IRC commands
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Chat (chatCommands, chatCommand', executeChat, cmdCtcp) where

import Client.Commands.Arguments.Spec
import Client.Commands.TabCompletion
import Client.Commands.Types
import Client.Commands.Window (parseFocus)
import Client.Message
import Client.State
import Client.State.Extensions (clientChatExtension)
import Client.State.Focus (focusNetwork, Focus(ChannelFocus), Subfocus(FocusInfo, FocusUsers))
import Client.State.Network (csNetwork, csUserInfo, sendMsg, NetworkState)
import Control.Applicative (liftA2, liftA3)
import Control.Lens (view, preview, views)
import Control.Monad (when)
import Data.Char (toUpper)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getZonedTime)
import Irc.Commands
import Irc.Identifier (Identifier, idText, mkId)
import Irc.Message (IrcMsg(Privmsg, Notice, Ctcp), Source(Source))
import Irc.RawIrcMsg (RawIrcMsg, parseRawIrcMsg)

chatCommands :: CommandSection
chatCommands = CommandSection "IRC commands"
  ------------------------------------------------------------------------

  [ Command
      ("join" :| ["j"])
      (liftA2 (,) (simpleToken "channels") (optionalArg (simpleToken "[keys]")))
      "\^BParameters:\^B\n\
      \\n\
      \    channels: Comma-separated list of channels\n\
      \    keys:     Comma-separated list of keys\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Join the given channels. When keys are provided, they should\n\
      \    occur in the same order as the channels.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /join #friends\n\
      \    /join #secret thekey\n\
      \    /join #secret1,#secret2 key1,key2\n\
      \\n\
      \\^BSee also:\^B channel, clear, part\n"
    $ NetworkCommand cmdJoin simpleNetworkTab

  , Command
      (pure "part")
      (remainingArg "reason")
      "\^BParameters:\^B\n\
      \\n\
      \    reason: Optional message sent to channel as part reason\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Part from the current channel.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /part\n\
      \    /part It's not me, it's you\n\
      \\n\
      \\^BSee also:\^B clear, join, quit\n"
    $ ChannelCommand cmdPart simpleChannelTab

  , Command
      (pure "msg")
      (liftA2 (,) (simpleToken "target") (remainingArg "message"))
      "\^BParameters:\^B\n\
      \\n\
      \    target:  Comma-separated list of nicknames and channels\n\
      \    message: Formatted message body\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a chat message to a user or a channel. On servers\n\
      \    with STATUSMSG support, the channel name can be prefixed\n\
      \    with a sigil to restrict the recipients to those with the\n\
      \    given mode.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /msg buddy I'm sending you a message.\n\
      \    /msg #friends This message is for the whole channel.\n\
      \    /msg him,her I'm chatting with two people.\n\
      \    /msg @#users This message is only for ops!\n\
      \\n\
      \\^BSee also:\^B notice, me, say\n"
    $ NetworkCommand cmdMsg simpleNetworkTab

  , Command
      (pure "me")
      (remainingArg "message")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Body of action message\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Sends an action message to the currently focused channel.\n\
      \    Most clients will render these messages prefixed with\n\
      \    only your nickname as though describing an action.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /me shrugs\n\
      \\n\
      \\^BSee also:\^B notice, msg, say\n"
    $ ChatCommand cmdMe simpleChannelTab

  , Command
      (pure "say")
      (remainingArg "message")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Body of message\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a message to the current chat window.  This can be useful\n\
      \    for sending a chat message with a leading '/' to the current\n\
      \    chat window.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /say /help is the right place to start!\n\
      \\n\
      \\^BSee also:\^B notice, me, msg\n"
    $ ChatCommand cmdSay simpleChannelTab

  , Command
      ("query" :| ["q"])
      (liftA2 (,) (simpleToken "target") (remainingArg "message"))
      "\^BParameters:\^B\n\
      \\n\
      \    target: Focus name\n\
      \    message: Optional message\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    This command switches the client focus to the given\n\
      \    target and optionally sends a message to that target.\n\
      \\n\
      \    Channel: \^_#channel\^_\n\
      \    Channel: \^_network\^_:\^_#channel\^_\n\
      \    User:    \^_nick\^_\n\
      \    User:    \^_network\^_:\^_nick\^_\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /q fn:#haskell\n\
      \    /q #haskell\n\
      \    /q lambdabot @messages\n\
      \    /q irc_friend How are you?\n\
      \\n\
      \\^BSee also:\^B msg channel focus\n"
    $ ClientCommand cmdQuery simpleClientTab

  , Command
      (pure "notice")
      (liftA2 (,) (simpleToken "target") (remainingArg "message"))
      "\^BParameters:\^B\n\
      \\n\
      \    target:  Comma-separated list of nicknames and channels\n\
      \    message: Formatted message body\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a chat notice to a user or a channel. On servers\n\
      \    with STATUSMSG support, the channel name can be prefixed\n\
      \    with a sigil to restrict the recipients to those with the\n\
      \    given mode. Notice messages were originally intended to be\n\
      \    used by bots. Different clients will render these in different\n\
      \    ways.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /notice buddy I'm sending you a message.\n\
      \    /notice #friends This message is for the whole channel.\n\
      \    /notice him,her I'm chatting with two people.\n\
      \    /notice @#users This message is only for ops!\n\
      \\n\
      \\^BSee also:\^B me, msg, say\n"
    $ NetworkCommand cmdNotice simpleNetworkTab

  , Command
      (pure "wallops")
      (remainingArg "message to +w users")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Formatted message body\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a network-wide WALLOPS message. These message go out\n\
      \    to users who have the 'w' usermode set.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /wallops Hi everyone, thanks for using this network!\n\
      \\n\
      \\^BSee also:\^B me, msg, say\n"
    $ NetworkCommand cmdWallops simpleNetworkTab

  , Command
      (pure "operwall")
      (remainingArg "message to +z opers")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Formatted message body\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Send a network-wide WALLOPS message to opers. These message go\n\
      \    out to opers who have the 'z' usermode set.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /operwall What's this even for?\n\
      \\n\
      \\^BSee also:\^B me, msg, say\n"
    $ NetworkCommand cmdOperwall simpleNetworkTab

  , Command
      (pure "ctcp")
      (liftA3 (,,) (simpleToken "target") (simpleToken "command") (remainingArg "arguments"))
      "\^BParameters:\^B\n\
      \\n\
      \    target:    Comma-separated list of nicknames and channels\n\
      \    command:   CTCP command name\n\
      \    arguments: CTCP command arguments\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Client-to-client protocol (CTCP) commands can be used\n\
      \    to query information from another user's client application\n\
      \    directly. Common CTCP commands include: ACTION, PING, VERSION,\n\
      \    USERINFO, CLIENTINFO, and TIME. glirc does not automatically\n\
      \    respond to CTCP commands.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /ctcp myfriend VERSION\n\
      \    /ctcp myfriend CLIENTINFO\n"
    $ NetworkCommand cmdCtcp simpleNetworkTab

  , Command
      (pure "nick")
      (simpleToken "nick")
      "\^BParameters:\^B\n\
      \\n\
      \    nick: New nickname\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Change your nickname on the currently focused server.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /nick guest123\n\
      \    /nick better_nick\n"
    $ NetworkCommand cmdNick simpleNetworkTab

  , Command
      (pure "away")
      (remainingArg "message")
      "\^BParameters:\^B\n\
      \\n\
      \    message: Optional away message\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Change your nickname on the currently focused server.\n\
      \    Omit the message parameter to clear your away status.\n\
      \    The away message is only used by the server to update\n\
      \    status in /whois and to provide automated responses.\n\
      \    It is not used by this client directly.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /away\n\
      \    /away Out getting some sun\n"
    $ NetworkCommand cmdAway simpleNetworkTab

  , Command
      (pure "names")
      (pure ())
      "\^BDescription:\^B\n\
      \\n\
      \    Show the user list for the current channel.\n\
      \    Detailed view (default key F2) shows full hostmask.\n\
      \    Hostmasks can be populated with /who #channel.\n\
      \    Press ESC to exit the userlist.\n\
      \\n\
      \\^BSee also:\^B channelinfo, masks\n"
    $ ChannelCommand cmdChanNames noChannelTab

  , Command
      (pure "channelinfo")
      (pure ())
      "\^BDescription:\^B\n\
      \\n\
      \    Show information about the current channel.\n\
      \    Press ESC to exit the channel info window.\n\
      \\n\
      \    Information includes topic, creation time, URL, and modes.\n\
      \\n\
      \\^BSee also:\^B masks, mode, topic, users\n"
    $ ChannelCommand cmdChannelInfo noChannelTab

  , Command
      (pure "knock")
      (liftA2 (,) (simpleToken "channel") (remainingArg "message"))
      "Request entry to an invite-only channel.\n"
    $ NetworkCommand cmdKnock simpleNetworkTab

  , Command
      (pure "quote")
      (remainingArg "raw IRC command")
      "Send a raw IRC command.\n"
    $ NetworkCommand cmdQuote simpleNetworkTab

  , Command
      (pure "monitor")
      (extensionArg "[+-CLS]" monitorArgs)
      "\^BSubcommands:\^B\n\
      \\n\
      \    /monitor + target[,target2]* - Add nicknames to monitor list\n\
      \    /monitor - target[,target2]* - Remove nicknames to monitor list\n\
      \    /monitor C                   - Clear monitor list\n\
      \    /monitor L                   - Show monitor list\n\
      \    /monitor S                   - Show status of nicknames on monitor list\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Monitor is a protocol for getting server-side notifications\n\
      \    when users become online/offline.\n"
    $ NetworkCommand cmdMonitor simpleNetworkTab

    ]

monitorArgs :: ClientState -> String -> Maybe (Args ClientState [String])
monitorArgs _ str =
  case toUpper <$> str of
    "+" -> Just (wrap '+' (simpleToken "target[,target2]*"))
    "-" -> Just (wrap '-' (simpleToken "target[,target2]*"))
    "C" -> Just (pure ["C"])
    "L" -> Just (pure ["L"])
    "S" -> Just (pure ["S"])
    _   -> Nothing
  where
    wrap c = fmap (\s -> [[c], s])

cmdMonitor :: NetworkCommand [String]
cmdMonitor cs st args =
  do sendMsg cs (ircMonitor (fmap Text.pack args))
     commandSuccess st

cmdChanNames :: ChannelCommand ()
cmdChanNames _ _ st _ = commandSuccess (changeSubfocus FocusUsers st)

cmdChannelInfo :: ChannelCommand ()
cmdChannelInfo _ _ st _ = commandSuccess (changeSubfocus FocusInfo st)

cmdKnock :: NetworkCommand (String, String)
cmdKnock cs st (chan,message) =
  do sendMsg cs (ircKnock (Text.pack chan) (Text.pack message))
     commandSuccess st

cmdJoin :: NetworkCommand (String, Maybe String)
cmdJoin cs st (channels, mbKeys) =
  do let network = view csNetwork cs
     let channelId = mkId (Text.pack (takeWhile (/=',') channels))
     sendMsg cs (ircJoin (Text.pack channels) (Text.pack <$> mbKeys))
     commandSuccess
        $ changeFocus (ChannelFocus network channelId) st

-- | @/query@ command. Takes a channel or nickname and switches
-- focus to that target on the current network.
cmdQuery :: ClientCommand (String, String)
cmdQuery st (target, msg) =
  case parseFocus (views clientFocus focusNetwork st) target of
    Just (ChannelFocus net tgt)

      | null msg -> commandSuccess st'

      | Just cs <- preview (clientConnection net) st ->
           do let tgtTxt = idText tgt
                  msgTxt = Text.pack msg
              chatCommand
                (ircPrivmsg tgtTxt msgTxt)
                (\src tgt1 -> Privmsg src tgt1 msgTxt)
                tgtTxt cs st'
      where
       firstTgt = mkId (Text.takeWhile (','/=) (idText tgt))
       st' = changeFocus (ChannelFocus net firstTgt) st

    _ -> commandFailureMsg "Bad target" st

-- | Implementation of @/ctcp@
cmdCtcp :: NetworkCommand (String, String, String)
cmdCtcp cs st (target, cmd, args) =
 do let cmdTxt = Text.toUpper (Text.pack cmd)
        argTxt = Text.pack args
        tgtTxt = Text.pack target

    let msg = "\^A" <> cmdTxt <>
              (if Text.null argTxt then "" else " " <> argTxt) <>
              "\^A"
    chatCommand
      (ircPrivmsg tgtTxt msg)
      (\src tgt -> Ctcp src tgt cmdTxt argTxt)
      tgtTxt cs st

-- | Implementation of @/wallops@
cmdWallops :: NetworkCommand String
cmdWallops cs st rest
  | null rest = commandFailureMsg "empty message" st
  | otherwise =
      do let restTxt = Text.pack rest
         sendMsg cs (ircWallops restTxt)
         commandSuccess st

-- | Implementation of @/operwall@
cmdOperwall :: NetworkCommand String
cmdOperwall cs st rest
  | null rest = commandFailureMsg "empty message" st
  | otherwise =
      do let restTxt = Text.pack rest
         sendMsg cs (ircOperwall restTxt)
         commandSuccess st

-- | Implementation of @/notice@
cmdNotice :: NetworkCommand (String, String)
cmdNotice cs st (target, rest)
  | null rest = commandFailureMsg "empty message" st
  | otherwise =
     do let restTxt = Text.pack rest
            tgtTxt = Text.pack target
        chatCommand
          (ircNotice tgtTxt restTxt)
          (\src tgt -> Notice src tgt restTxt)
          tgtTxt cs st

-- | Implementation of @/msg@
cmdMsg :: NetworkCommand (String, String)
cmdMsg cs st (target, rest)
  | null rest = commandFailureMsg "empty message" st
  | otherwise =
     do let restTxt = Text.pack rest
            tgtTxt = Text.pack target
        chatCommand
          (ircPrivmsg tgtTxt restTxt)
          (\src tgt -> Privmsg src tgt restTxt)
          tgtTxt cs st
        


-- | Common logic for @/msg@ and @/notice@
chatCommand ::
  RawIrcMsg {- ^ irc command -} ->
  (Source -> Identifier -> IrcMsg) ->
  Text {- ^ targets -} ->
  NetworkState         ->
  ClientState          ->
  IO CommandResult
chatCommand ircMsg mkmsg tgtsTxt cs st
  | any Text.null tgtTxts = commandFailureMsg "empty target" st
  | otherwise =
   do sendMsg cs ircMsg
      st' <- chatCommand' mkmsg tgtTxts cs st
      commandSuccess st'
  where
    tgtTxts = Text.split (','==) tgtsTxt

-- | Common logic for @/msg@ and @/notice@ returning the client state
chatCommand' ::
  (Source -> Identifier -> IrcMsg) ->
  [Text] {- ^ targets  -} ->
  NetworkState         ->
  ClientState          ->
  IO ClientState
chatCommand' con targetTxts cs st =
  do now <- getZonedTime
     let targetIds = mkId <$> targetTxts
         !myNick = Source (view csUserInfo cs) ""
         network = view csNetwork cs
         entries = [ (targetId,
                          ClientMessage
                          { _msgTime = now
                          , _msgNetwork = network
                          , _msgBody = IrcBody (con myNick targetId)
                          })
                       | targetId <- targetIds ]

     return $! foldl' (\acc (targetId, entry) ->
                        recordChannelMessage network targetId entry acc)
                      st
                      entries

-- | Implementation of @/quote@. Parses arguments as a raw IRC command and
-- sends to the current network.
cmdQuote :: NetworkCommand String
cmdQuote cs st rest =
  case parseRawIrcMsg (Text.pack (dropWhile (' '==) rest)) of
    Nothing  -> commandFailureMsg "failed to parse raw IRC command" st
    Just raw ->
      do sendMsg cs raw
         commandSuccess st

cmdAway :: NetworkCommand String
cmdAway cs st rest =
  do sendMsg cs (ircAway (Text.pack rest))
     commandSuccess st

cmdNick :: NetworkCommand String
cmdNick cs st nick =
  do sendMsg cs (ircNick (Text.pack nick))
     commandSuccess st

cmdPart :: ChannelCommand String
cmdPart channelId cs st rest =
  do let msg = rest
     sendMsg cs (ircPart channelId (Text.pack msg))
     commandSuccess st

-- | This command is equivalent to chatting without a command. The primary use
-- at the moment is to be able to send a leading @/@ to chat easily.
cmdSay :: ChannelCommand String
cmdSay _ _ st rest = executeChat rest st

-- | Implementation of @/me@
cmdMe :: ChannelCommand String
cmdMe channelId cs st rest =
  do now <- getZonedTime
     let actionTxt = Text.pack ("\^AACTION " ++ rest ++ "\^A")
         !myNick = Source (view csUserInfo cs) ""
         network = view csNetwork cs
         entry = ClientMessage
                    { _msgTime = now
                    , _msgNetwork = network
                    , _msgBody = IrcBody (Ctcp myNick channelId "ACTION" (Text.pack rest))
                    }
     sendMsg cs (ircPrivmsg (idText channelId) actionTxt)
     commandSuccess
       $! recordChannelMessage network channelId entry st

-- | Treat the current text input as a chat message and send it.
executeChat ::
  String           {- ^ chat message   -} ->
  ClientState      {- ^ client state   -} ->
  IO CommandResult {- ^ command result -}
executeChat msg st =
  case view clientFocus st of
    ChannelFocus network channel
      | Just !cs <- preview (clientConnection network) st ->
          do now <- getZonedTime
             let msgTxt = Text.pack $ takeWhile (/='\n') msg
                 tgtTxt = idText channel

             (st1,allow) <- clientChatExtension network tgtTxt msgTxt st

             when allow (sendMsg cs (ircPrivmsg tgtTxt msgTxt))

             let myNick = Source (view csUserInfo cs) ""
                 entry = ClientMessage
                   { _msgTime    = now
                   , _msgNetwork = network
                   , _msgBody    = IrcBody (Privmsg myNick channel msgTxt) }
             commandSuccess $! recordChannelMessage network channel entry st1

    _ -> commandFailureMsg "cannot send chat messages to this window" st
