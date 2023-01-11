{-# Language RankNTypes, OverloadedStrings #-}
{-|
Module      : Client.EventLoop.Actions
Description : Programmable keyboard actions
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.EventLoop.Actions
  ( Action(..)
  , KeyMap
  , keyToAction
  , initialKeyMap
  , addKeyBinding
  , removeKeyBinding
  , keyMapEntries

  -- * Keys as text
  , parseKey
  , prettyModifierKey
  , actionName
  ) where

import Config.Schema.Spec (anyAtomSpec, customSpec, HasSpec(..))
import Control.Applicative (Alternative(empty))
import Control.Lens (Lens', (^.), non', (?~), set, At(at), AsEmpty(_Empty))
import Data.Char (showLitChar)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty.Input.Events
import Text.Read (readMaybe)

-- | Actions that can be invoked using the keyboard.
data Action

  = ActBackspace
  | ActDelete
  | ActLeft
  | ActRight
  | ActHome
  | ActEnd
  | ActOlderLine
  | ActNewerLine
  | ActScrollUp
  | ActScrollDown
  | ActScrollUpSmall
  | ActScrollDownSmall
  | ActBackWord
  | ActForwardWord

  | ActYank
  | ActKillHome
  | ActKillEnd
  | ActKillWordBack
  | ActKillWordForward
  | ActToggle

  | ActBold
  | ActColor
  | ActItalic
  | ActUnderline
  | ActStrikethrough
  | ActReverseVideo
  | ActMonospace
  | ActClearFormat
  | ActInsertEnter
  | ActDigraph

  | ActRetreatFocus
  | ActAdvanceFocus
  | ActAdvanceNetwork
  | ActJumpToActivity
  | ActJumpPrevious
  | ActJump Char

  | ActTabComplete
  | ActTabCompleteBack

  | ActEnter
  | ActReset
  | ActRefresh
  | ActCommand Text
  | ActInsert Char
  | ActIgnored
  deriving (Eq, Ord, Read, Show)

-- | Lookup table for keyboard events to actions. Use with
-- keyToAction.
newtype KeyMap = KeyMap (Map [Modifier] (Map Key Action))
  deriving (Show)

keyMapEntries :: KeyMap -> [([Modifier], Key, Action)]
keyMapEntries (KeyMap m) =
  [ (mods, k, a)
     | (mods, m1) <- Map.toList m
     , (k,a) <- Map.toList m1
     ]

instance HasSpec Action where
  anySpec = customSpec "action" anyAtomSpec
          $ \a -> case HashMap.lookup a actionInfos of
                    Nothing -> Left "unknown action"
                    Just x  -> Right (fst x)


-- | Names and default key bindings for each action.
--
-- Note that Jump, Insert and Ignored are excluded. These will
-- be computed on demand by keyToAction.
actionInfos :: HashMap Text (Action, [([Modifier],Key)])
actionInfos =
  let norm = (,) [     ]
      ctrl = (,) [MCtrl]
      meta = (,) [MMeta] in

  HashMap.fromList

  [("delete"            , (ActDelete           , [ctrl (KChar 'd'), norm KDel]))
  ,("backspace"         , (ActBackspace        , [norm KBS]))
  ,("home"              , (ActHome             , [norm KHome, ctrl (KChar 'a')]))
  ,("end"               , (ActEnd              , [norm KEnd , ctrl (KChar 'e')]))
  ,("kill-home"         , (ActKillHome         , [ctrl (KChar 'u')]))
  ,("kill-end"          , (ActKillEnd          , [ctrl (KChar 'k')]))
  ,("yank"              , (ActYank             , [ctrl (KChar 'y')]))
  ,("toggle"            , (ActToggle           , [ctrl (KChar 't')]))
  ,("kill-word-left"    , (ActKillWordBack     , [ctrl (KChar 'w'), meta KBS]))
  ,("kill-word-right"   , (ActKillWordForward  , [meta (KChar 'd')]))

  ,("bold"              , (ActBold             , [ctrl (KChar 'b')]))
  ,("color"             , (ActColor            , [ctrl (KChar 'c')]))
  ,("italic"            , (ActItalic           , [ctrl (KChar ']')]))
  ,("strikethrough"     , (ActStrikethrough    , [ctrl (KChar '^')]))
  ,("underline"         , (ActUnderline        , [ctrl (KChar '_')]))
  ,("clear-format"      , (ActClearFormat      , [ctrl (KChar 'o')]))
  ,("reverse-video"     , (ActReverseVideo     , [ctrl (KChar 'v')]))
  ,("monospace"         , (ActMonospace        , [ctrl (KChar 'q')]))

  ,("insert-newline"    , (ActInsertEnter      , [meta KEnter]))
  ,("insert-digraph"    , (ActDigraph          , [meta (KChar 'k')]))

  ,("next-window"       , (ActAdvanceFocus     , [ctrl (KChar 'n')]))
  ,("prev-window"       , (ActRetreatFocus     , [ctrl (KChar 'p')]))
  ,("next-network"      , (ActAdvanceNetwork   , [ctrl (KChar 'x')]))
  ,("refresh"           , (ActRefresh          , [ctrl (KChar 'l')]))
  ,("jump-to-activity"  , (ActJumpToActivity   , [meta (KChar 'a')]))
  ,("jump-to-previous"  , (ActJumpPrevious     , [meta (KChar 's')]))

  ,("reset"             , (ActReset            , [norm KEsc]))

  ,("left-word"         , (ActBackWord         , [meta KLeft, meta (KChar 'b')]))
  ,("right-word"        , (ActForwardWord      , [meta KRight, meta (KChar 'f')]))
  ,("left"              , (ActLeft             , [norm KLeft]))
  ,("right"             , (ActRight            , [norm KRight]))
  ,("up"                , (ActOlderLine        , [norm KUp]))
  ,("down"              , (ActNewerLine        , [norm KDown]))
  ,("scroll-up"         , (ActScrollUp         , [norm KPageUp]))
  ,("scroll-down"       , (ActScrollDown       , [norm KPageDown]))
  ,("scroll-up-small"   , (ActScrollUpSmall    , [meta KPageUp]))
  ,("scroll-down-small" , (ActScrollDownSmall  , [meta KPageDown]))
  ,("enter"             , (ActEnter            , [norm KEnter]))
  ,("word-complete-back", (ActTabCompleteBack  , [norm KBackTab]))
  ,("word-complete"     , (ActTabComplete      , [norm (KChar '\t')]))
  ]


actionNames :: Map Action Text
actionNames = Map.fromList
  [ (action, name) | (name, (action,_)) <- HashMap.toList actionInfos ]


-- | Render action as human-readable text.
actionName :: Action -> Text
actionName (ActCommand txt) = "command: " <> txt
actionName a = Map.findWithDefault (Text.pack (show a)) a actionNames


keyMapLens :: [Modifier] -> Key -> Lens' KeyMap (Maybe Action)
keyMapLens mods key f (KeyMap m) =
  KeyMap <$> (at (normalizeModifiers mods) . non' _Empty . at key) f m


-- | Lookup the action to perform in response to a particular key event.
keyToAction ::
  KeyMap     {- ^ actions         -} ->
  [Modifier] {- ^ jump modifier   -} ->
  Text       {- ^ window names    -} ->
  [Modifier] {- ^ actual modifier -} ->
  Key        {- ^ key             -} ->
  Action     {- ^ action          -}
keyToAction _ jumpMods names mods (KChar c)
  | normalizeModifiers jumpMods == normalizeModifiers mods
  , Text.singleton c `Text.isInfixOf` names = ActJump c
keyToAction m _ _ modifier key =
  case m ^. keyMapLens modifier key of
    Just a -> a
    Nothing | KChar c <- key, null modifier -> ActInsert c
            | otherwise                     -> ActIgnored


-- | Bind a keypress event to a new action.
addKeyBinding ::
  [Modifier] {- ^ modifiers -} ->
  Key        {- ^ key       -} ->
  Action     {- ^ action    -} ->
  KeyMap     {- ^ actions   -} ->
  KeyMap
addKeyBinding mods k a = keyMapLens mods k ?~ a

-- | Unbind the action associated with a key.
removeKeyBinding ::
  [Modifier] {- ^ modifiers -} ->
  Key        {- ^ key       -} ->
  KeyMap     {- ^ actions   -} ->
  KeyMap
removeKeyBinding mods k = set (keyMapLens mods k) Nothing


normalizeModifiers :: [Modifier] -> [Modifier]
normalizeModifiers = nub . sort


-- | Default key bindings
initialKeyMap :: KeyMap
initialKeyMap = KeyMap $
  Map.fromListWith Map.union $

   ([], Map.fromList
          [ (KFun 2, ActCommand "toggle-detail")
          , (KFun 3, ActCommand "toggle-activity-bar")
          , (KFun 4, ActCommand "toggle-metadata")
          , (KFun 5, ActCommand "toggle-layout")
          , (KFun 6, ActCommand "toggle-editor")
          , (KFun 7, ActCommand "toggle-edit-lock")
          ])
    :

    [ (mods, Map.singleton k act)
      | (act, mks) <- HashMap.elems actionInfos
      , (mods, k)  <- mks
      ]


parseKey :: String -> Maybe ([Modifier], Key)
parseKey = getCompose . go
  where
    modifier x   = Compose (Just ([x], ()))
    liftMaybe mb = Compose ((,)[] <$> mb)
    go str =
      case str of
        "Space"     -> pure (KChar ' ')
        "Tab"       -> pure (KChar '\t')
        "BackTab"   -> pure KBackTab
        "Enter"     -> pure KEnter
        "Home"      -> pure KHome
        "End"       -> pure KEnd
        "Esc"       -> pure KEsc
        "PageUp"    -> pure KPageUp
        "PageDown"  -> pure KPageDown
        "Backspace" -> pure KBS
        "Delete"    -> pure KDel
        "Left"      -> pure KLeft
        "Right"     -> pure KRight
        "Up"        -> pure KUp
        "Down"      -> pure KDown
        [c]         -> pure (KChar c)
        'F':xs      -> KFun <$> liftMaybe (readMaybe xs)
        'C':'-':xs  -> modifier MCtrl  *> go xs
        'M':'-':xs  -> modifier MMeta  *> go xs
        'S':'-':xs  -> modifier MShift *> go xs
        'A':'-':xs  -> modifier MAlt   *> go xs
        _           -> empty


prettyModifierKey :: [Modifier] -> Key -> String
prettyModifierKey mods k
  = foldr prettyModifier (prettyKey k) mods

prettyModifier :: Modifier -> ShowS
prettyModifier MCtrl  = showString "C-"
prettyModifier MMeta  = showString "M-"
prettyModifier MShift = showString "S-"
prettyModifier MAlt   = showString "A-"

prettyKey :: Key -> String
prettyKey (KChar ' ') = "Space"
prettyKey (KChar '\t') = "Tab"
prettyKey (KChar c) = showLitChar c "" -- escapes anything non-ascii
prettyKey (KFun n)  = 'F' : show n
prettyKey KBackTab  = "BackTab"
prettyKey KEnter    = "Enter"
prettyKey KEsc      = "Esc"
prettyKey KHome     = "Home"
prettyKey KEnd      = "End"
prettyKey KPageUp   = "PageUp"
prettyKey KPageDown = "PageDn"
prettyKey KDel      = "Delete"
prettyKey KBS       = "Backspace"
prettyKey KLeft     = "Left"
prettyKey KRight    = "Right"
prettyKey KUp       = "Up"
prettyKey KDown     = "Down"
prettyKey k         = show k
