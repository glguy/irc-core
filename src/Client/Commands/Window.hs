{-# Language OverloadedStrings, TemplateHaskell #-}
{-|
Module      : Client.Commands.Window
Description : Window command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Window (windowCommands, parseFocus, focusNames) where

import Client.Commands.Arguments.Spec
import Client.Commands.Docs (windowDocs, cmdDoc)
import Client.Commands.TabCompletion
import Client.Commands.Types
import Client.Commands.WordCompletion (plainWordCompleteMode)
import Client.Mask (buildMask)
import Client.State
import Client.State.EditBox qualified as Edit
import Client.State.Focus
import Client.State.Network (csChannels)
import Client.State.Window (windowClear, wlText, winMessages, winHidden, winActivityFilter, winName, activityFilterStrings, readActivityFilter)
import Control.Applicative (liftA2)
import Control.Exception (SomeException, Exception(displayException), try)
import Control.Lens
import Data.Foldable (Foldable(foldl', toList))
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.List ((\\), nub)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.IO qualified as LText
import Irc.Identifier (Identifier, idText, mkId)

windowCommands :: CommandSection
windowCommands = CommandSection "Window management"
  ------------------------------------------------------------------------

  [ Command
      (pure "focus")
      (liftA2 (,) (simpleToken "network") (optionalArg (simpleToken "[target]")))
      $(windowDocs `cmdDoc` "focus")
    $ ClientCommand cmdFocus tabFocus

  , Command
      ("c" :| ["channel"])
      (simpleToken "focus")
      $(windowDocs `cmdDoc` "channel")
    $ ClientCommand cmdChannel tabChannel

  , Command
      (pure "clear")
      (optionalArg (liftA2 (,) (simpleToken "[network]") (optionalArg (simpleToken "[channel]"))))
      $(windowDocs `cmdDoc` "clear")
    $ WindowCommand cmdClear (\rev _ -> tabFocus rev)

  , Command
      (pure "windows")
      (optionalArg (simpleToken "[kind]"))
      $(windowDocs `cmdDoc` "windows")
    $ ClientCommand cmdWindows tabWindows

  , Command
      (pure "splits")
      (remainingArg "focuses")
      $(windowDocs `cmdDoc` "splits")
    $ ClientCommand cmdSplits tabSplits

  , Command
      (pure "splits+")
      (remainingArg "focuses")
      $(windowDocs `cmdDoc` "splits")
    $ ClientCommand cmdSplitsAdd tabSplits

  , Command
      (pure "splits-")
      (remainingArg "focuses")
      $(windowDocs `cmdDoc` "splits")
    $ ClientCommand cmdSplitsDel tabActiveSplits

  , Command
      (pure "ignore")
      (remainingArg "masks")
      $(windowDocs `cmdDoc` "ignore")
    $ ClientCommand cmdIgnore tabIgnore

  , Command
      (pure "grep")
      (remainingArg "regular-expression")
      $(windowDocs `cmdDoc` "grep")
    $ ClientCommand cmdGrep simpleClientTab

  , Command
      (pure "dump")
      (simpleToken "filename")
      $(windowDocs `cmdDoc` "dump")
    $ WindowCommand cmdDump (\rev _ -> simpleClientTab rev)

  , Command
      (pure "mentions")
      (pure ())
      $(windowDocs `cmdDoc` "mentions")
    $ ClientCommand cmdMentions noClientTab

  , Command
      (pure "setwindow")
      (simpleToken ("hide|show" ++ concatMap ('|':) activityFilterStrings))
      $(windowDocs `cmdDoc` "setwindow")
    $ WindowCommand cmdSetWindow tabSetWindow

  , Command
      (pure "setname")
      (optionalArg (simpleToken "[letter]"))
      $(windowDocs `cmdDoc` "setname")
    $ WindowCommand cmdSetWindowName (\rev _ -> noClientTab rev)
  ]

cmdSetWindowName :: WindowCommand (Maybe String)
cmdSetWindowName focus st arg =
  -- unset current name so that it becomes available
  let mbSt1 = failover (clientWindows . ix focus . winName) (\_ -> Nothing) st in
  case mbSt1 of
    Nothing -> commandFailureMsg "no current window" st
    Just st1 ->
      let next = clientNextWindowName (clientWindowHint focus st) st
          mbName =
            case arg of
              Just [n] | n `elem` clientWindowNames st -> Right n
              Just _ -> Left "invalid name"
              Nothing
                | next /= '\0' -> Right next
                | otherwise -> Left "no free names" in
      case mbName of
        Left e -> commandFailureMsg e st
        Right name ->
          let unset n = if n == Just name then Nothing else n in
          commandSuccess
            $ set  (clientWindows . ix focus . winName) (Just name)
            $ over (clientWindows . each     . winName) unset
            $ st1

cmdSetWindow :: WindowCommand String
cmdSetWindow focus st cmd =
  case mbFun of
    Nothing -> commandFailureMsg "bad window setting" st
    Just f ->
      case failover (clientWindows . ix focus) f st of
        Nothing -> commandFailureMsg "no such window" st
        Just st' -> commandSuccess st'
  where
    mbFun =
      case cmd of
        "show"    -> Just (set winHidden False)
        "hide"    -> Just (set winName Nothing . set winHidden True)
        other     -> set winActivityFilter <$> readActivityFilter other

tabSetWindow :: Bool {- ^ reversed -} -> WindowCommand String
tabSetWindow isReversed _ st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = "hide":"show": map Text.pack activityFilterStrings

-- | Implementation of @/grep@
cmdGrep :: ClientCommand String
cmdGrep st str
  | null str  = commandSuccess (set clientRegex Nothing st)
  | otherwise =
      case buildMatcher str of
        Nothing -> commandFailureMsg "bad grep" st
        Just  r -> commandSuccess (set clientRegex (Just r) st)

-- | Implementation of @/windows@ command. Set subfocus to Windows.
cmdWindows :: ClientCommand (Maybe String)
cmdWindows st arg =
  case arg of
    Nothing         -> success AllWindows
    Just "networks" -> success NetworkWindows
    Just "channels" -> success ChannelWindows
    Just "users"    -> success UserWindows
    _               -> commandFailureMsg errmsg st
  where
    errmsg = "/windows expected networks, channels, or users"
    success x =
      commandSuccess (changeSubfocus (FocusWindows x) st)

-- | Implementation of @/mentions@ command. Set subfocus to Mentions.
cmdMentions :: ClientCommand ()
cmdMentions st _ = commandSuccess (changeSubfocus FocusMentions st)

cmdIgnore :: ClientCommand String
cmdIgnore st rest =
  case mkId <$> Text.words (Text.pack rest) of
    [] -> commandSuccess (changeSubfocus FocusIgnoreList st)
    xs -> commandSuccess st2
      where
        (newIgnores, st1) = (clientIgnores <%~ updateIgnores) st
        st2 = set clientIgnoreMask (buildMask (toList newIgnores)) st1

        updateIgnores :: HashSet Identifier -> HashSet Identifier
        updateIgnores s = foldl' updateIgnore s xs

        updateIgnore s x = over (contains x) not s

-- | Complete the nickname at the current cursor position using the
-- userlist for the currently focused channel (if any)
tabIgnore :: Bool {- ^ reversed -} -> ClientCommand String
tabIgnore isReversed st _ =
  simpleTabCompletion mode hint completions isReversed st
  where
    hint          = activeNicks st
    completions   = currentCompletionList st ++ views clientIgnores toList st
    mode          = currentNickCompletionMode st

-- | Implementation of @/splits@
cmdSplits :: ClientCommand String
cmdSplits st str =
  withSplitFocuses st str $ \args ->
    commandSuccess (setExtraFocus (nub args) st)


-- | Implementation of @/splits+@. When no focuses are provided
-- the current focus is used instead.
cmdSplitsAdd :: ClientCommand String
cmdSplitsAdd st str =
  withSplitFocuses st str $ \args ->
    let args'
          | null args = [(view clientFocus st, view clientSubfocus st)]
          | otherwise = args
        extras = nub (args' ++ view clientExtraFocus st)

    in commandSuccess (setExtraFocus extras st)

-- | Implementation of @/splits-@. When no focuses are provided
-- the current focus is used instead.
cmdSplitsDel :: ClientCommand String
cmdSplitsDel st str =
  withSplitFocuses st str $ \args ->
    let args'
          | null args = [(view clientFocus st, view clientSubfocus st)]
          | otherwise = args
        extras = view clientExtraFocus st \\ args'

    in commandSuccess (setExtraFocus extras st)

withSplitFocuses ::
  ClientState                   ->
  String                        ->
  ([(Focus, Subfocus)] -> IO CommandResult) ->
  IO CommandResult
withSplitFocuses st str k =
  case mb of
    Nothing   -> commandFailureMsg "unable to parse arguments" st
    Just args -> k [(x, FocusMessages) | x <- args]
  where
    mb = traverse
           (parseFocus (views clientFocus focusNetwork st))
           (words str)

cmdFocus :: ClientCommand (String, Maybe String)
cmdFocus st (network, mbChannel)
  | network == "*" = commandSuccess (changeFocus Unfocused st)
  | otherwise =
     case mbChannel of
       Nothing ->
         let focus = NetworkFocus (Text.pack network) in
         commandSuccess (changeFocus focus st)
       Just channel ->
         let focus = ChannelFocus (Text.pack network) (mkId (Text.pack channel)) in
         commandSuccess
           $ changeFocus focus st

tabWindows :: Bool -> ClientCommand String
tabWindows isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = ["networks","channels","users"] :: [Text]

-- | Tab completion for @/splits-@. This completes only from the list of active
-- entries in the splits list.
tabActiveSplits :: Bool -> ClientCommand String
tabActiveSplits isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = currentNetSplits <> currentSplits
    currentSplits = [renderSplitFocus x | (x, FocusMessages) <- view clientExtraFocus st]
    currentNetSplits =
      [ idText chan
        | (ChannelFocus net chan, FocusMessages) <- view clientExtraFocus st
        , views clientFocus focusNetwork st == Just net
        ]

-- | When used on a channel that the user is currently
-- joined to this command will clear the messages but
-- preserve the window. When used on a window that the
-- user is not joined to this command will delete the window.
cmdClear :: WindowCommand (Maybe (String, Maybe String))
cmdClear focusDefault st args =
  case args of
    Nothing                      -> clearFocus focusDefault
    Just ("*",     Nothing     ) -> clearFocus Unfocused
    Just (network, Nothing     ) -> clearFocus (NetworkFocus (Text.pack network))
    Just (network, Just "*"    ) -> clearNetworkWindows network
    Just (network, Just channel) -> clearFocus (ChannelFocus (Text.pack network) (mkId (Text.pack channel)))
  where
    clearNetworkWindows network
      = commandSuccess
      $ foldl' (flip clearFocus1) st
      $ filter (\x -> focusNetwork x == Just (Text.pack network))
      $ views clientWindows Map.keys st

    clearFocus focus = commandSuccess (clearFocus1 focus st)

    clearFocus1 focus st' = focusEffect (windowEffect st')
      where
        windowEffect = over (clientWindows . at focus)
                           (if isActive then fmap windowClear else const Nothing)

        focusEffect
          | noChangeNeeded    = id
          | prevExists        = changeFocus prev
          | otherwise         = advanceFocus
          where
            noChangeNeeded    = isActive || view clientFocus st' /= focus
            prevExists        = has (clientWindows . ix prev) st'

            prev              = view clientPrevFocus st

        isActive =
          case focus of
            Unfocused                    -> False
            NetworkFocus network         -> has (clientConnection network) st'
            ChannelFocus network channel -> has (clientConnection network
                                                .csChannels . ix channel) st'

-- | Tab completion for @/splits[+]@. When given no arguments this
-- populates the current list of splits, otherwise it tab completes
-- all of the currently available windows.
tabSplits :: Bool -> ClientCommand String
tabSplits isReversed st rest

  -- If no arguments, populate the current splits
  | all (' '==) rest =
     let cmd = unwords $ "/splits"
                       : [Text.unpack (renderSplitFocus x) | (x, FocusMessages) <- view clientExtraFocus st]
         newline = Edit.endLine cmd
     in commandSuccess (set (clientTextBox . Edit.line) newline st)

  -- Tab complete the available windows. Accepts either fully qualified
  -- window names or current network names without the ':'
  | otherwise =
     let completions = currentNet <> allWindows
         allWindows  = renderSplitFocus <$> views clientWindows Map.keys st
         currentNet  = case views clientFocus focusNetwork st of
                         Just net -> idText <$> channelWindowsOnNetwork net st
                         Nothing  -> []
     in simpleTabCompletion plainWordCompleteMode [] completions isReversed st

-- | Render a entry from splits back to the textual format.
renderSplitFocus :: Focus -> Text
renderSplitFocus Unfocused          = "*"
renderSplitFocus (NetworkFocus x)   = x <> ":"
renderSplitFocus (ChannelFocus x y) = x <> ":" <> idText y

focusNames :: ClientState -> [Text]
focusNames st = currentNet <> allWindows
  where
    allWindows  = renderSplitFocus <$> views clientWindows Map.keys st
    currentNet  = case views clientFocus focusNetwork st of
                    Just net -> idText <$> channelWindowsOnNetwork net st
                    Nothing  -> []

-- | When tab completing the first parameter of the focus command
-- the current networks are used.
tabFocus :: Bool -> ClientCommand String
tabFocus isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    networks   = map mkId $ HashMap.keys $ view clientConnections st
    params     = words $ uncurry take $ clientLine st

    completions =
      case params of
        [_cmd,_net]      -> networks
        [_cmd,net,_chan] -> channelWindowsOnNetwork (Text.pack net) st
        _                -> []

-- | @/channel@ command. Takes a channel or nickname and switches
-- focus to that target on the current network.
cmdChannel :: ClientCommand String
cmdChannel st channel =
  case parseFocus (views clientFocus focusNetwork st) channel of
    Just focus -> commandSuccess (changeFocus focus st)
    Nothing    -> commandFailureMsg "No current network" st

-- | Tab completion for @/channel@. Tab completion uses pre-existing
-- windows.
tabChannel ::
  Bool {- ^ reversed order -} ->
  ClientCommand String
tabChannel isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] (focusNames st) isReversed st

-- | Return the list of identifiers for open channel windows on
-- the given network name.
channelWindowsOnNetwork ::
  Text         {- ^ network              -} ->
  ClientState  {- ^ client state         -} ->
  [Identifier] {- ^ open channel windows -}
channelWindowsOnNetwork network st =
  [ chan | ChannelFocus net chan <- Map.keys (view clientWindows st)
         , net == network ]

-- | Implementation of @/dump@. Writes detailed contents of focused buffer
-- to the given filename.
cmdDump :: WindowCommand String
cmdDump focus st fp =
  do res <- try (LText.writeFile fp (LText.unlines outputLines))
     case res of
       Left e  -> commandFailureMsg (Text.pack (displayException (e :: SomeException))) st
       Right{} -> commandSuccess st

  where
    outputLines
      = reverse
      $ clientFilter st id
      $ toListOf (clientWindows . ix focus . winMessages . each . wlText) st
