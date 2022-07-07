{-# Language OverloadedStrings #-}
{-|
Module      : Client.Commands.Window
Description : Window command implementations
Copyright   : (c) Eric Mertens, 2016-2020
License     : ISC
Maintainer  : emertens@gmail.com
-}

module Client.Commands.Window (windowCommands, parseFocus) where

import           Client.Commands.Arguments.Spec
import           Client.Commands.TabCompletion
import           Client.Commands.Types
import           Client.Commands.WordCompletion
import           Client.Mask (buildMask)
import           Client.State
import           Client.State.Focus
import           Client.State.Network
import           Client.State.Window (windowClear, wlText, winMessages, winHidden, winSilent, winName)
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Data.Foldable
import           Data.List ((\\), nub)
import qualified Client.State.EditBox as Edit
import           Data.HashSet (HashSet)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import           Irc.Identifier

windowCommands :: CommandSection
windowCommands = CommandSection "Window management"
  ------------------------------------------------------------------------

  [ Command
      (pure "focus")
      (liftA2 (,) (simpleToken "network") (optionalArg (simpleToken "[target]")))
      "Change the focused window.\n\
      \\n\
      \When only \^Bnetwork\^B is specified this switches to the network status window.\n\
      \When \^Bnetwork\^B and \^Btarget\^B are specified this switches to that chat window.\n\
      \\n\
      \Nickname and channels can be specified in the \^Btarget\^B parameter.\n\
      \See also: /query (aliased /c /channel) to switch to a target on the current network.\n"
    $ ClientCommand cmdFocus tabFocus

  , Command
      ("c" :| ["channel"])
      (simpleToken "focus")
      "\^BParameters:\^B\n\
      \\n\
      \    focuses: Focus name\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    This command sets the current window focus. When\n\
      \    no network is specified, the current network will\n\
      \    be used.\n\
      \\n\
      \    Client:  *\n\
      \    Network: \^_network\^_:\n\
      \    Channel: \^_#channel\^_\n\
      \    Channel: \^_network\^_:\^_#channel\^_\n\
      \    User:    \^_nick\^_\n\
      \    User:    \^_network\^_:\^_nick\^_\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /c fn:#haskell\n\
      \    /c #haskell\n\
      \    /c fn:\n\
      \    /c *:\n\
      \\n\
      \\^BSee also:\^B focus\n"
    $ ClientCommand cmdChannel tabChannel

  , Command
      (pure "clear")
      (optionalArg (liftA2 (,) (simpleToken "[network]") (optionalArg (simpleToken "[channel]"))))
      "Clear a window.\n\
      \\n\
      \If no arguments are provided the current window is cleared.\n\
      \If \^Bnetwork\^B is provided the that network window is cleared.\n\
      \If \^Bnetwork\^B and \^Bchannel\^B are provided that chat window is cleared.\n\
      \If \^Bnetwork\^B is provided and \^Bchannel\^B is \^B*\^O all windows for that network are cleared.\n\
      \\n\
      \If a window is cleared and no longer active that window will be removed from the client.\n"
    $ ClientCommand cmdClear tabFocus

  , Command
      (pure "windows")
      (optionalArg (simpleToken "[kind]"))
      "Show a list of all windows with an optional argument to limit the kinds of windows listed.\n\
      \\n\
      \\^Bkind\^O: one of \^Bnetworks\^O, \^Bchannels\^O, \^Busers\^O\n\
      \\n"
    $ ClientCommand cmdWindows tabWindows

  , Command
      (pure "splits")
      (remainingArg "focuses")
      "\^BParameters:\^B\n\
      \\n\
      \    focuses: List of focus names\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    This command sents the set of focuses that will always\n\
      \    be visible, even when unfocused. When the client is focused\n\
      \    to an active network, the network can be omitted when\n\
      \    specifying a focus. If no focuses are listed, they will\n\
      \    all be cleared.\n\
      \\n\
      \    Client:  *\n\
      \    Network: \^_network\^_:\n\
      \    Channel: \^_#channel\^_\n\
      \    Channel: \^_network\^_:\^_#channel\^_\n\
      \    User:    \^_nick\^_\n\
      \    User:    \^_network\^_:\^_nick\^_\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /splits * fn:#haskell fn:chanserv\n\
      \    /splits #haskell #haskell-lens nickserv\n\
      \    /splits\n\
      \\n\
      \\^BSee also:\^B splits+, splits-\n"
    $ ClientCommand cmdSplits tabSplits

  , Command
      (pure "splits+")
      (remainingArg "focuses")
      "Add windows to the splits list. Omit the list of focuses to add the\
      \ current window.\n\
      \\n\
      \\^Bfocuses\^B: space delimited list of focus names.\n\
      \\n\
      \Client:  *\n\
      \Network: \^BNETWORK\^B\n\
      \Channel: \^BNETWORK\^B:\^B#CHANNEL\^B\n\
      \User:    \^BNETWORK\^B:\^BNICK\^B\n\
      \\n\
      \If the network part is omitted, the current network will be used.\n"
    $ ClientCommand cmdSplitsAdd tabSplits

  , Command
      (pure "splits-")
      (remainingArg "focuses")
      "Remove windows from the splits list. Omit the list of focuses to\
      \ remove the current window.\n\
      \\n\
      \\^Bfocuses\^B: space delimited list of focus names.\n\
      \\n\
      \Client:  *\n\
      \Network: \^BNETWORK\^B\n\
      \Channel: \^BNETWORK\^B:\^B#CHANNEL\^B\n\
      \User:    \^BNETWORK\^B:\^BNICK\^B\n\
      \\n\
      \If the network part is omitted, the current network will be used.\n"
    $ ClientCommand cmdSplitsDel tabActiveSplits

  , Command
      (pure "ignore")
      (remainingArg "masks")
      "\^BParameters:\^B\n\
      \\n\
      \    masks: List of masks\n\
      \\n\
      \\^BDescription:\^B\n\
      \\n\
      \    Toggle the soft-ignore on each of the space-delimited given\n\
      \    nicknames. Ignores can use \^B*\^B (many) and \^B?\^B (one) wildcards.\n\
      \    Masks can be of the form: nick[[!user]@host]\n\
      \    Masks use a case-insensitive comparison.\n\
      \\n\
      \    If no masks are specified the current ignore list is displayed.\n\
      \\n\
      \\^BExamples:\^B\n\
      \\n\
      \    /ignore\n\
      \    /ignore nick1 nick2 nick3\n\
      \    /ignore nick@host\n\
      \    /ignore nick!user@host\n\
      \    /ignore *@host\n\
      \    /ignore *!baduser@*\n"
    $ ClientCommand cmdIgnore tabIgnore

  , Command
      (pure "grep")
      (remainingArg "regular-expression")
      "Set the persistent regular expression.\n\
      \\n\
      \\^BFlags:\^B\n\
      \    -A n Show n messages after match\n\
      \    -B n Show n messages before match\n\
      \    -C n Show n messages before and after match\n\
      \    -F   Use plain-text match instead of regular expression\n\
      \    -i   Case insensitive match\n\
      \    -v   Invert pattern match\n\
      \    -m n Limit results to n matches\n\
      \    --   Stop processing flags\n\
      \\n\
      \Clear the regular expression by calling this without an argument.\n\
      \\n\
      \\^B/grep\^O is case-sensitive.\n"
    $ ClientCommand cmdGrep simpleClientTab

  , Command
      (pure "dump")
      (simpleToken "filename")
      "Dump current buffer to file.\n"
    $ ClientCommand cmdDump simpleClientTab

  , Command
      (pure "mentions")
      (pure ())
      "Show a list of all message that were highlighted as important.\n\
      \\n\
      \When using \^B/grep\^B the important messages are those matching\n\
      \the regular expression instead.\n"
    $ ClientCommand cmdMentions noClientTab

  , Command
      (pure "setwindow")
      (simpleToken "hide|show|loud|silent")
      "Set window property.\n\
      \\n\
      \\^Bloud\^B / \^Bsilent\^B\n\
      \    Toggles if window activity appears in the status bar.\n\
      \\n\
      \\^Bshow\^B / \^Bhide\^B\n\
      \    Toggles if window appears in window command shortcuts.\n"
    $ ClientCommand cmdSetWindow tabSetWindow

  , Command
      (pure "setname")
      (optionalArg (simpleToken "[letter]"))
      "Set window shortcut letter. If no letter is provided the next available\n\
      \letter will automatically be assigned.\n\
      \\n\
      \Available letters are configured in the 'window-names' configuration setting.\n"
    $ ClientCommand cmdSetWindowName noClientTab

  ]

cmdSetWindowName :: ClientCommand (Maybe String)
cmdSetWindowName st arg =
  -- unset current name so that it becomes available
  let mbSt1 = failover (clientWindows . ix (view clientFocus st) . winName) (\_ -> Nothing) st in
  case mbSt1 of
    Nothing -> commandFailureMsg "no current window" st
    Just st1 ->
      let next = clientNextWindowName (clientWindowHint (view clientFocus st) st) st
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
            $ set  (clientWindows . ix (view clientFocus st) . winName) (Just name)
            $ over (clientWindows . each                     . winName) unset
            $ st1

cmdSetWindow :: ClientCommand String
cmdSetWindow st cmd =
  case mbFun of
    Nothing -> commandFailureMsg "bad window setting" st
    Just f ->
      case failover (clientWindows . ix (view clientFocus st)) f st of
        Nothing -> commandFailureMsg "no such window" st
        Just st' -> commandSuccess st'
  where
    mbFun =
      case cmd of
        "show"   -> Just (set winHidden False)
        "hide"   -> Just (set winName Nothing . set winHidden True)
        "loud"   -> Just (set winSilent False)
        "silent" -> Just (set winSilent True)
        _        -> Nothing

tabSetWindow :: Bool {- ^ reversed -} -> ClientCommand String
tabSetWindow isReversed st _ =
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = ["hide", "show", "loud", "silent"] :: [Text]

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

-- | Parses a single focus name given a default network.
parseFocus ::
  Maybe Text {- ^ default network    -} ->
  String {- ^ @[network:]target@ -} ->
  Maybe Focus
parseFocus mbNet x =
  case break (==':') x of
    ("*","")     -> pure Unfocused
    (net,_:"")   -> pure (NetworkFocus (Text.pack net))
    (net,_:chan) -> pure (ChannelFocus (Text.pack net) (mkId (Text.pack chan)))
    (chan,"")    -> mbNet <&> \net ->
                    ChannelFocus net (mkId (Text.pack chan))

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
cmdClear :: ClientCommand (Maybe (String, Maybe String))
cmdClear st args =
  case args of
    Nothing                      -> clearFocus (view clientFocus st)
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
  simpleTabCompletion plainWordCompleteMode [] completions isReversed st
  where
    completions = currentNet <> allWindows
    allWindows  = renderSplitFocus <$> views clientWindows Map.keys st
    currentNet  = case views clientFocus focusNetwork st of
                    Just net -> idText <$> channelWindowsOnNetwork net st
                    Nothing  -> []

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
cmdDump :: ClientCommand String
cmdDump st fp =
  do res <- try (LText.writeFile fp (LText.unlines outputLines))
     case res of
       Left e  -> commandFailureMsg (Text.pack (displayException (e :: SomeException))) st
       Right{} -> commandSuccess st

  where
    focus = view clientFocus st

    outputLines
      = reverse
      $ clientFilter st id
      $ toListOf (clientWindows . ix focus . winMessages . each . wlText) st
