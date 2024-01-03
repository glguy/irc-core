{-# Language BangPatterns, OverloadedStrings, TemplateHaskell #-}
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
import Client.Commands.Docs (chatDocs, cmdDoc)

chatCommands :: CommandSection
chatCommands = CommandSection "IRC commands"
  ------------------------------------------------------------------------

  [ Command
      ("join" :| ["j"])
      (liftA2 (,) (simpleToken "channels") (optionalArg (simpleToken "[keys]")))
      $(chatDocs `cmdDoc` "join")
    $ NetworkCommand cmdJoin simpleNetworkTab

  , Command
      (pure "part")
      (remainingArg "reason")
      $(chatDocs `cmdDoc` "part")
    $ ChannelCommand cmdPart simpleChannelTab

  , Command
      (pure "msg")
      (liftA2 (,) (simpleToken "target") (remainingArg "message"))
      $(chatDocs `cmdDoc` "msg")
    $ NetworkCommand cmdMsg simpleNetworkTab

  , Command
      (pure "me")
      (remainingArg "message")
      $(chatDocs `cmdDoc` "me")
    $ ChatCommand cmdMe simpleChannelTab

  , Command
      (pure "say")
      (remainingArg "message")
      $(chatDocs `cmdDoc` "say")
    $ ChatCommand cmdSay simpleChannelTab

  , Command
      ("query" :| ["q"])
      (liftA2 (,) (simpleToken "target") (remainingArg "message"))
      $(chatDocs `cmdDoc` "query")
    $ ClientCommand cmdQuery simpleClientTab

  , Command
      (pure "notice")
      (liftA2 (,) (simpleToken "target") (remainingArg "message"))
      $(chatDocs `cmdDoc` "notice")
    $ NetworkCommand cmdNotice simpleNetworkTab

  , Command
      (pure "wallops")
      (remainingArg "message to +w users")
      $(chatDocs `cmdDoc` "wallops")
    $ NetworkCommand cmdWallops simpleNetworkTab

  , Command
      (pure "operwall")
      (remainingArg "message to +z opers")
      $(chatDocs `cmdDoc` "operwall")
    $ NetworkCommand cmdOperwall simpleNetworkTab

  , Command
      (pure "ctcp")
      (liftA3 (,,) (simpleToken "target") (simpleToken "command") (remainingArg "arguments"))
      $(chatDocs `cmdDoc` "ctcp")
    $ NetworkCommand cmdCtcp simpleNetworkTab

  , Command
      (pure "nick")
      (simpleToken "nick")
      $(chatDocs `cmdDoc` "nick")
    $ NetworkCommand cmdNick simpleNetworkTab

  , Command
      (pure "away")
      (remainingArg "message")
      $(chatDocs `cmdDoc` "away")
    $ NetworkCommand cmdAway simpleNetworkTab

  , Command
      (pure "names")
      (pure ())
      $(chatDocs `cmdDoc` "names")
    $ ChannelCommand cmdChanNames noChannelTab

  , Command
      (pure "channelinfo")
      (pure ())
      $(chatDocs `cmdDoc` "channelinfo")
    $ ChannelCommand cmdChannelInfo noChannelTab

  , Command
      (pure "knock")
      (liftA2 (,) (simpleToken "channel") (remainingArg "message"))
      $(chatDocs `cmdDoc` "knock")
    $ NetworkCommand cmdKnock simpleNetworkTab

  , Command
      (pure "quote")
      (remainingArg "raw IRC command")
      $(chatDocs `cmdDoc` "quote")
    $ NetworkCommand cmdQuote simpleNetworkTab

  , Command
      (pure "monitor")
      (extensionArg "[+-CLS]" monitorArgs)
      $(chatDocs `cmdDoc` "monitor")
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
