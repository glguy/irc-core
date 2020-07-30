{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Channel
Description : Channel management command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Channel (channelCommands) where

import           Client.Commands.Arguments.Spec
import           Client.Commands.TabCompletion
import           Client.Commands.Types
import           Client.Commands.WordCompletion
import           Client.State
import           Client.State.Channel
import           Client.State.Focus
import           Client.State.Network
import           Client.UserHost
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable (traverse_)
import           Data.List.Split (chunksOf)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Client.State.EditBox as Edit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import           Irc.Commands
import           Irc.Modes
import           Irc.UserInfo
import           Irc.Identifier
import           LensUtils (setStrict)

channelCommands :: CommandSection
channelCommands = CommandSection "IRC channel management"

  [ Command
      (pure "mode")
      (fromMaybe [] <$> optionalArg (extensionArg "[modes]" modeParamArgs))
      "Sets IRC modes.\n\
      \\n\
      \Examples:\n\
      \Setting a ban:           /mode +b *!*@hostname\n\
      \Removing a quiet:        /mode -q *!*@hostname\n\
      \Voicing two users:       /mode +vv user1 user2\n\
      \Demoting an op to voice: /mode +v-o user1 user1\n\
      \\n\
      \When executed in a network window, mode changes are applied to your user.\n\
      \When executed in a channel window, mode changes are applied to the channel.\n\
      \\n\
      \This command has parameter sensitive tab-completion.\n\
      \\n\
      \See also: /masks /channelinfo\n"
    $ NetworkCommand cmdMode tabMode

  , Command
      (pure "masks")
      (simpleToken "mode")
      "Show mask lists for current channel.\n\
      \\n\
      \Common \^Bmode\^B values:\n\
      \\^Bb\^B: bans\n\
      \\^Bq\^B: quiets\n\
      \\^BI\^B: invite exemptions (op view only)\n\
      \\^Be\^B: ban exemption (op view only)s\n\
      \\n\
      \To populate the mask lists for the first time use: /mode \^Bmode\^B\n\
      \\n\
      \See also: /mode\n"
    $ ChannelCommand cmdMasks noChannelTab

  , Command
      (pure "invite")
      (simpleToken "nick")
      "Invite a user to the current channel.\n"
    $ ChannelCommand cmdInvite simpleChannelTab

  , Command
      (pure "topic")
      (remainingArg "message")
      "Set the topic on the current channel.\n\
      \\n\
      \Tab-completion with no \^Bmessage\^B specified will load the current topic for editing.\n"
    $ ChannelCommand cmdTopic tabTopic

  , Command
      (pure "kick")
      (liftA2 (,) (simpleToken "nick") (remainingArg "reason"))
      "Kick a user from the current channel.\n\
      \\n\
      \See also: /kickban /remove\n"
    $ ChannelCommand cmdKick simpleChannelTab

  , Command
      (pure "kickban")
      (liftA2 (,) (simpleToken "nick") (remainingArg "reason"))
      "Ban and kick a user from the current channel.\n\
      \\n\
      \Users are banned by hostname match.\n\
      \See also: /kick /remove\n"
    $ ChannelCommand cmdKickBan simpleChannelTab

  , Command
      (pure "remove")
      (liftA2 (,) (simpleToken "nick") (remainingArg "reason"))
      "Remove a user from the current channel.\n\
      \\n\
      \Remove works like /kick except it results in a PART.\n\
      \See also: /kick /kickban\n"
    $ ChannelCommand cmdRemove simpleChannelTab

  ]

cmdRemove :: ChannelCommand (String, String)
cmdRemove channelId cs st (who,reason) =
  do let msg = Text.pack reason
         cmd = ircRemove channelId (Text.pack who) msg
     cs' <- sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st

cmdKick :: ChannelCommand (String, String)
cmdKick channelId cs st (who,reason) =
  do let msg = Text.pack reason
         cmd = ircKick channelId (Text.pack who) msg
     cs' <- sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st


cmdKickBan :: ChannelCommand (String, String)
cmdKickBan channelId cs st (who,reason) =
  do let msg = Text.pack reason

         whoTxt     = Text.pack who

         mask = renderUserInfo (computeBanUserInfo (mkId whoTxt) cs)
         cmds = [ ircMode channelId ["b", mask]
                , ircKick channelId whoTxt msg
                ]
     cs' <- sendModeration channelId cmds cs
     commandSuccessUpdateCS cs' st

cmdInvite :: ChannelCommand String
cmdInvite channelId cs st nick =
  do let freeTarget = has (csChannels . ix channelId . chanModes . ix 'g') cs
         cmd = ircInvite (Text.pack nick) channelId
     cs' <- if freeTarget
              then cs <$ sendMsg cs cmd
              else sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st

commandSuccessUpdateCS :: NetworkState -> ClientState -> IO CommandResult
commandSuccessUpdateCS cs st =
  do let network = view csNetwork cs
     commandSuccess
       $ setStrict (clientConnection network) cs st

cmdMasks :: ChannelCommand String
cmdMasks channel cs st rest =
  case rest of
    [mode] | mode `elem` view (csModeTypes . modesLists) cs ->

        do let connecting = has (csPingStatus . _PingConnecting) cs
               listLoaded = has (csChannels . ix channel . chanLists . ix mode) cs
           unless (connecting || listLoaded)
             (sendMsg cs (ircMode channel [Text.singleton mode]))

           commandSuccess (changeSubfocus (FocusMasks mode) st)

    _ -> commandFailureMsg "unknown mask mode" st

computeBanUserInfo :: Identifier -> NetworkState    -> UserInfo
computeBanUserInfo who cs =
  case view (csUser who) cs of
    Nothing                     -> UserInfo who "*" "*"
    Just (UserAndHost _ host _) -> UserInfo "*" "*" host

cmdTopic :: ChannelCommand String
cmdTopic channelId cs st rest =
  do sendTopic channelId (Text.pack rest) cs
     commandSuccess st

tabTopic ::
  Bool {- ^ reversed -} ->
  ChannelCommand String
tabTopic _ channelId cs st rest

  | all (==' ') rest
  , Just topic <- preview (csChannels . ix channelId . chanTopic) cs =
     do let textBox = set Edit.line (Edit.endLine $ "/topic " ++ Text.unpack topic)
        commandSuccess (over clientTextBox textBox st)

  | otherwise = commandFailure st

cmdMode :: NetworkCommand [String]
cmdMode cs st xs = modeCommand (Text.pack <$> xs) cs st

modeCommand ::
  [Text] {- mode parameters -} ->
  NetworkState                 ->
  ClientState                  ->
  IO CommandResult
modeCommand modes cs st =
  case view clientFocus st of

    NetworkFocus _ ->
      do sendMsg cs (ircMode (view csNick cs) modes)
         commandSuccess st

    ChannelFocus _ chan ->
      case modes of
        [] -> success False [[]]
        flags:params ->
          case splitModes (view csModeTypes cs) flags params of
            Nothing -> commandFailureMsg "failed to parse modes" st
            Just parsedModes ->
              success needOp (unsplitModes <$> chunksOf (view csModeCount cs) parsedModes')
              where
                parsedModes'
                  | useChanServ chan cs = filter (not . isOpMe) parsedModes
                  | otherwise           = parsedModes

                needOp = not (all isPublicChannelMode parsedModes)
      where
        isOpMe (True, 'o', param) = mkId param == view csNick cs
        isOpMe _                  = False

        success needOp argss =
          do let cmds = ircMode chan <$> argss
             cs' <- if needOp
                      then sendModeration chan cmds cs
                      else cs <$ traverse_ (sendMsg cs) cmds
             commandSuccessUpdateCS cs' st

    _ -> commandFailure st

tabMode :: Bool -> NetworkCommand String
tabMode isReversed cs st rest =
  case view clientFocus st of

    ChannelFocus _ channel
      | flags:params     <- Text.words (Text.pack rest)
      , Just parsedModes <- splitModes (view csModeTypes cs) flags params
      , let parsedModesWithParams =
              [ (pol,mode) | (pol,mode,arg) <- parsedModes, not (Text.null arg) ]
      , (pol,mode):_      <- drop (paramIndex-3) parsedModesWithParams
      , let (hint, completions) = computeModeCompletion pol mode channel cs st
      -> simpleTabCompletion plainWordCompleteMode hint completions isReversed st

    _ -> commandFailure st

  where
    paramIndex = length $ words $ uncurry take $ clientLine st

modeParamArgs :: ClientState -> String -> Maybe (Args ClientState [String])
modeParamArgs st str =
  case view clientFocus st of
    Unfocused      -> Nothing
    NetworkFocus _ -> Just (pure [str])
    ChannelFocus net _ ->

         -- determine current mode types
      do cs <- preview (clientConnection net) st
         let types = view csModeTypes cs

         -- parse the list of modes being set
         flags <- splitModes types (Text.pack str) []

         -- generate the argument specification
         let (req,opt) = foldr (countFlags types) ([],[]) flags
         return ((str:) <$> tokenList req (map (++"?") opt))

-- | This function computes the list of required and optional parameters
-- corresponding to the flags that have been entered.
countFlags ::
  ModeTypes           {- ^ network's mode behaviors              -} ->
  (Bool, Char, Text)  {- ^ polarity mode-letter unused-parameter -} ->
  ([String],[String]) {- ^ required-names optional-names         -} ->
  ([String],[String]) {- ^ required-names optional-names         -}
countFlags types (pol, flag, _)
  |        flag `elem` view modesLists       types = addOpt
  | pol && flag `elem` view modesSetArg      types = addReq
  |        flag `elem` view modesAlwaysArg   types = addReq
  | elemOf (modesPrefixModes . folded . _1) flag types = addReq
  | otherwise                                      = id
  where
    addReq (req,opt) = ((flag:" param"):req,opt)
    addOpt ([] ,opt) = ([], (flag:" param"):opt)
    addOpt (req,opt) = ((flag:" param"):req,opt)


-- | Use the *!*@host masks of users for channel lists when setting list modes
--
-- Use the channel's mask list for removing modes
--
-- Use the nick list otherwise
computeModeCompletion ::
  Bool {- ^ mode polarity -} ->
  Char {- ^ mode          -} ->
  Identifier {- ^ channel -} ->
  NetworkState    ->
  ClientState ->
  ([Identifier],[Identifier]) {- ^ (hint, complete) -}
computeModeCompletion pol mode channel cs st
  | mode `elem` view modesLists modeSettings =
        if pol then ([],usermasks) else ([],masks)
  | otherwise = (activeNicks st, nicks)
  where
    modeSettings = view csModeTypes cs
    nicks = HashMap.keys (view (csChannels . ix channel . chanUsers) cs)

    masks = mkId <$> HashMap.keys (view (csChannels . ix channel . chanLists . ix mode) cs)

    usermasks =
      [ mkId ("*!*@" <> host)
        | nick <- HashMap.keys (view (csChannels . ix channel . chanUsers) cs)
        , UserAndHost _ host _ <- toListOf (csUsers . ix nick) cs
        ]

-- | Predicate for mode commands that can be performed without ops
isPublicChannelMode :: (Bool, Char, Text) -> Bool
isPublicChannelMode (True, 'b', param) = Text.null param -- query ban list
isPublicChannelMode (True, 'q', param) = Text.null param -- query quiet list
isPublicChannelMode _                  = False
