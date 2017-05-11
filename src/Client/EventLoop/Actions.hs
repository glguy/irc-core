{-|
Module      : Client.EventLoop.Actions
Description : Programmable keyboard actions
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.EventLoop.Actions (Action(..), keyToAction) where

import           Graphics.Vty.Input.Events
import           Data.List

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

keyToAction ::
  [Char]     {- ^ window names -} ->
  [Modifier] {- ^ key modifier -} ->
  Key        {- ^ key          -} ->
  Action     {- ^ action       -}
keyToAction winNames modifier key =
  case modifier of
    [MCtrl] ->
      case key of
        KChar 'd' -> ActDelete
        KChar 'a' -> ActHome
        KChar 'e' -> ActEnd
        KChar 'u' -> ActKillHome
        KChar 'k' -> ActKillEnd
        KChar 'y' -> ActYank
        KChar 't' -> ActToggle
        KChar 'w' -> ActKillWordBack
        KChar 'b' -> ActBold
        KChar 'c' -> ActColor
        KChar ']' -> ActItalic
        KChar '_' -> ActUnderline
        KChar 'o' -> ActClearFormat
        KChar 'v' -> ActReverseVideo
        KChar 'p' -> ActRetreatFocus
        KChar 'n' -> ActAdvanceFocus
        KChar 'x' -> ActAdvenceNetwork
        KChar 'l' -> ActRefresh
        _         -> ActIgnored

    [MMeta] ->
      case key of
        KChar c   | Just i <- elemIndex c winNames -> ActJump i
        KEnter    -> ActInsertEnter
        KBS       -> ActKillWordBack
        KChar 'd' -> ActKillWordForward
        KChar 'b' -> ActBackWord
        KChar 'f' -> ActForwardWord
        KLeft     -> ActBackWord
        KRight    -> ActForwardWord
        KChar 'a' -> ActJumpToActivity
        KChar 's' -> ActJumpPrevious
        KChar 'k' -> ActDigraph
        _         -> ActIgnored

    [] -> -- no modifier
      case key of
        KEsc       -> ActReset
        KBS        -> ActBackspace
        KDel       -> ActDelete
        KLeft      -> ActLeft
        KRight     -> ActRight
        KHome      -> ActHome
        KEnd       -> ActEnd
        KUp        -> ActOlderLine
        KDown      -> ActNewerLine
        KPageUp    -> ActScrollUp
        KPageDown  -> ActScrollDown

        KEnter     -> ActEnter
        KBackTab   -> ActTabCompleteBack
        KChar '\t' -> ActTabComplete

        -- toggles
        KFun 2     -> ActToggleDetail
        KFun 3     -> ActToggleActivityBar
        KFun 4     -> ActToggleHideMeta

        KChar c    -> ActInsert c
        _          -> ActIgnored
