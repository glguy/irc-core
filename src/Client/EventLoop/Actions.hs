{-# Language OverloadedStrings #-}
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
  , keyMapEntries
  , parseKey
  ) where

import           Graphics.Vty.Input.Events
import           Config.Schema.Spec
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Read

-- | Actions that can be invoked using the keyboard.
data Action

  = ActDelete
  | ActHome
  | ActEnd
  | ActKillHome
  | ActKillEnd
  | ActYank
  | ActToggle
  | ActKillWordBack
  | ActBold
  | ActColor
  | ActItalic
  | ActUnderline
  | ActClearFormat
  | ActReverseVideo
  | ActRetreatFocus
  | ActAdvanceFocus
  | ActAdvenceNetwork
  | ActRefresh
  | ActIgnored

  | ActJump Int
  | ActInsertEnter
  | ActKillWordForward
  | ActBackWord
  | ActForwardWord
  | ActJumpToActivity
  | ActJumpPrevious
  | ActDigraph

  | ActReset
  | ActBackspace
  | ActLeft
  | ActRight
  | ActOlderLine
  | ActNewerLine
  | ActScrollUp
  | ActScrollDown
  | ActEnter
  | ActTabCompleteBack
  | ActTabComplete
  | ActInsert Char
  | ActToggleDetail
  | ActToggleActivityBar
  | ActToggleHideMeta
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

instance Spec Action where
  valuesSpec = customSpec "action" anyAtomSpec $ \a ->
                fst <$> HashMap.lookup a actionInfos


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

  [("delete"            , (ActDelete           , [meta (KChar 'd'), norm KDel]))
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
  ,("underline"         , (ActUnderline        , [ctrl (KChar '_')]))
  ,("clear-format"      , (ActClearFormat      , [ctrl (KChar 'o')]))
  ,("reverse-video"     , (ActReverseVideo     , [ctrl (KChar 'v')]))

  ,("insert-newline"    , (ActInsertEnter      , [meta KEnter]))
  ,("insert-digraph"    , (ActDigraph          , [meta (KChar 'k')]))

  ,("next-window"       , (ActRetreatFocus     , [ctrl (KChar 'n')]))
  ,("prev-window"       , (ActAdvanceFocus     , [ctrl (KChar 'p')]))
  ,("next-network"      , (ActAdvenceNetwork   , [ctrl (KChar 'x')]))
  ,("refresh"           , (ActRefresh          , [ctrl (KChar 'l')]))
  ,("jump-to-activity"  , (ActJumpToActivity   , [meta (KChar 'a')]))
  ,("jump-previous"     , (ActJumpPrevious     , [meta (KChar 's')]))

  ,("reset"             , (ActReset            , [norm KEsc]))

  ,("left-word"         , (ActBackWord         , [meta KLeft, meta (KChar 'b')]))
  ,("right-word"        , (ActForwardWord      , [meta KRight, meta (KChar 'f')]))
  ,("left"              , (ActLeft             , [norm KLeft]))
  ,("right"             , (ActRight            , [norm KRight]))
  ,("history-up"        , (ActOlderLine        , [norm KUp]))
  ,("history-down"      , (ActNewerLine        , [norm KDown]))
  ,("scroll-up"         , (ActScrollUp         , [norm KPageUp]))
  ,("scroll-down"       , (ActScrollDown       , [norm KPageDown]))
  ,("enter"             , (ActEnter            , [norm KEnter]))
  ,("word-complete-back", (ActTabCompleteBack  , [norm KBackTab]))
  ,("word-complete"     , (ActTabComplete      , [norm (KChar '\t')]))
  ,("toggle-detail"     , (ActToggleDetail     , [norm (KFun 2)]))
  ,("toggle-activity"   , (ActToggleActivityBar, [norm (KFun 3)]))
  ,("toggle-metadata"   , (ActToggleHideMeta   , [norm (KFun 4)]))
  ]


-- | Lookup the action to perform in response to a particular key event.
keyToAction ::
  KeyMap     {- ^ actions      -} ->
  Text       {- ^ window names -} ->
  [Modifier] {- ^ key modifier -} ->
  Key        {- ^ key          -} ->
  Action     {- ^ action       -}
keyToAction _ names [MMeta] (KChar c)
  | Just i <- Text.findIndex (c==) names = ActJump i
keyToAction (KeyMap m) names modifier key =
  case Map.lookup key =<< Map.lookup modifier m of
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
addKeyBinding mods k a (KeyMap m) = KeyMap $
  Map.alter (Just . maybe (Map.singleton k a) (Map.insert k a)) mods m


-- | Default key bindings
initialKeyMap :: KeyMap
initialKeyMap = KeyMap $
  Map.fromListWith Map.union
    [ (mods, Map.singleton k act)
      | (act, mks) <- HashMap.elems actionInfos
      , (mods, k)  <- mks
      ]


parseKey :: String -> Maybe ([Modifier], Key)
parseKey [c] = Just ([], KChar c)

parseKey ('F':xs) =
  do i <- readMaybe xs
     Just ([], KFun i)

parseKey ('C':'-':xs) =
  do (m,k) <- parseKey xs
     Just (MCtrl:m, k)

parseKey ('M':'-':xs) =
  do (m,k) <- parseKey xs
     Just (MMeta:m, k)

parseKey ('A':'-':xs) =
  do (m,k) <- parseKey xs
     Just (MAlt:m, k)

parseKey ('S':'-':xs) =
  do (m,k) <- parseKey xs
     Just (MShift:m, k)

parseKey _ = Nothing
