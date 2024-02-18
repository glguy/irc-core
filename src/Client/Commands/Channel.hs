{-# Language OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Client.Commands.Channel
Description : Channel management command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Channel (channelCommands) where

import Client.Commands.Arguments.Spec
import Client.Commands.Docs (chanopDocs, cmdDoc)
import Client.Commands.TabCompletion (activeNicks, noChannelTab, simpleChannelTab, simpleTabCompletion)
import Client.Commands.Types
import Client.Commands.WordCompletion (plainWordCompleteMode)
import Client.State
import Client.State.Channel (chanLists, chanModes, chanTopic, chanUsers)
import Client.State.EditBox qualified as Edit
import Client.State.Focus
import Client.State.Network
import Client.UserHost ( UserAndHost(UserAndHost) )
import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HashMap
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Irc.Commands (ircInvite, ircKick, ircMode, ircPrivmsg, ircRemove)
import Irc.Identifier (Identifier, mkId, idText)
import Irc.Modes
import Irc.Message (isNickChar)
import Irc.UserInfo (UserInfo(UserInfo), renderUserInfo)

channelCommands :: CommandSection
channelCommands = CommandSection "IRC channel management"

  [ Command
      (pure "mode")
      (fromMaybe [] <$> optionalArg (extensionArg "[modes]" modeParamArgs))
      $(chanopDocs `cmdDoc` "mode")
    $ MaybeChatCommand cmdMode tabMode

  , Command
      (pure "masks")
      (simpleToken "mode")
      $(chanopDocs `cmdDoc` "masks")
    $ ChannelCommand cmdMasks noChannelTab

  , Command
      (pure "invite")
      (simpleToken "nick")
      $(chanopDocs `cmdDoc` "invite")
    $ ChannelCommand cmdInvite simpleChannelTab

  , Command
      (pure "topic")
      (remainingArg "message")
      $(chanopDocs `cmdDoc` "topic")
    $ ChannelCommand cmdTopic tabTopic

  , Command
      (pure "kick")
      (liftA2 (,) (simpleToken "nick") (remainingArg "reason"))
      $(chanopDocs `cmdDoc` "kick")
    $ ChannelCommand cmdKick simpleChannelTab

  , Command
      (pure "kickban")
      (liftA2 (,) (simpleToken "nick") (remainingArg "reason"))
      $(chanopDocs `cmdDoc` "kickban")
    $ ChannelCommand cmdKickBan simpleChannelTab

  , Command
      (pure "quiet")
      (simpleToken "nick|mask")
      $(chanopDocs `cmdDoc` "quiet")
    $ ChannelCommand cmdQuiet simpleChannelTab

  , Command
      (pure "remove")
      (liftA2 (,) (simpleToken "nick") (remainingArg "reason"))
      $(chanopDocs `cmdDoc` "remove")
    $ ChannelCommand cmdRemove simpleChannelTab

  , Command
      (pure "op")
      (optionalArg $ simpleToken "[nick]")
      $(chanopDocs `cmdDoc` "op")
    $ ChannelCommand (cmdStatus "OP" "+o") simpleChannelTab

  , Command
      (pure "deop")
      (optionalArg $ simpleToken "[nick]")
      $(chanopDocs `cmdDoc` "deop")
    $ ChannelCommand (cmdStatus "DEOP" "-o") simpleChannelTab

  , Command
      (pure "voice")
      (optionalArg $ simpleToken "[nick]")
      $(chanopDocs `cmdDoc` "voice")
    $ ChannelCommand (cmdStatus "VOICE" "+v") simpleChannelTab

  , Command
      (pure "devoice")
      (optionalArg $ simpleToken "[nick]")
      $(chanopDocs `cmdDoc` "devoice")
    $ ChannelCommand (cmdStatus "DEVOICE" "-v") simpleChannelTab
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

cmdQuiet :: ChannelCommand String
cmdQuiet channelId cs st who
  | elem 'q' $ view (csModeTypes . modesLists) cs = do
    let
      whoTxt = Text.pack who
      mask = if Text.all isNickChar whoTxt then renderUserInfo (computeBanUserInfo (mkId whoTxt) cs) else whoTxt
    cs' <- sendModeration channelId [ircMode channelId ["q", mask]] cs
    commandSuccessUpdateCS cs' st
  | otherwise = commandFailureMsg "no list mode q on network" st

cmdInvite :: ChannelCommand String
cmdInvite channelId cs st nick =
  do let freeTarget = has (csChannels . ix channelId . chanModes . ix 'g') cs
         cmd = ircInvite (Text.pack nick) channelId
     cs' <- if freeTarget
              then cs <$ sendMsg cs cmd
              else sendModeration channelId [cmd] cs
     commandSuccessUpdateCS cs' st

cmdMasks :: ChannelCommand String
cmdMasks channel cs st rest =
  case rest of
    [mode] | mode `elem` view (csModeTypes . modesLists) cs ->

        do let connecting = has (csPingStatus . _PingConnecting) cs
               listLoaded = maybe False (has (chanLists . ix mode)) (csChannelFresh channel cs)
           let cs' = recreateChanIfStale channel cs
           unless (connecting || listLoaded)
             (sendMsg cs' (ircMode channel [Text.singleton mode]))
           commandSuccessUpdateCS cs' (changeSubfocus (FocusMasks (view csNetwork cs) channel mode) st)

    _ -> commandFailureMsg "unknown mask mode" st

cmdStatus :: Text -> Text -> ChannelCommand (Maybe String)
cmdStatus servCmd modeChg chan cs st target
  | useChanServ chan cs = do
    let command = [servCmd, idText chan] ++ maybeToList targetText
    sendMsg cs $ ircPrivmsg "ChanServ" $ Text.unwords command
    commandSuccessUpdateCS cs st
  | otherwise = do
    let target' = fromMaybe (idText $ view csNick cs) targetText
    sendMsg cs $ ircMode chan [modeChg, target']
    commandSuccessUpdateCS cs st
  where
    targetText = Text.pack <$> target

computeBanUserInfo :: Identifier -> NetworkState    -> UserInfo
computeBanUserInfo who cs =
  case view (csUser who) cs of
    -- TODO: Maybe have multiple ways of computing this.
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

cmdMode :: MaybeChatCommand [String]
cmdMode chan cs st xs = modeCommand chan (Text.pack <$> xs) cs st

modeCommand ::
  Maybe Identifier {- channel -} ->
  [Text] {- mode parameters -}   ->
  NetworkState                   ->
  ClientState                    ->
  IO CommandResult
modeCommand maybeChan modes cs st =
  case maybeChan of
    Nothing ->
      do sendMsg cs (ircMode (view csNick cs) modes)
         commandSuccess st

    Just chan ->
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

tabMode :: Bool -> MaybeChatCommand String
tabMode isReversed maybeChan cs st rest =
  case maybeChan of
    Just channel
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

modeParamArgs :: ArgsContext -> String -> Maybe (Args ArgsContext [String])
modeParamArgs ArgsContext{argsContextSt=st, argsContextFocus=focus} str =
  case focus of
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
        if pol then ([],usermasks <> accounts) else ([],masks)
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
    accounts =
      [ mkId ("$a:" <> account)
        | nick <- HashMap.keys (view (csChannels . ix channel . chanUsers) cs)
        , UserAndHost _ _ account <- toListOf (csUsers . ix nick) cs
        , not (Text.null account)
        ]

-- | Predicate for mode commands that can be performed without ops
isPublicChannelMode :: (Bool, Char, Text) -> Bool
isPublicChannelMode (True, 'b', param) = Text.null param -- query ban list
isPublicChannelMode (True, 'q', param) = Text.null param -- query quiet list
isPublicChannelMode _                  = False
