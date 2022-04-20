{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Client.State.EditBox
Description : Console-mode text box
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides support for the text operations important for
providing a text input in the IRC client. It tracks user input
history, tab completion history, and provides many update operations
which are mapped to the keyboard in "Client.EventLoop".

-}

module Client.State.EditBox
  ( -- * Edit box type
    EditBox
  , defaultEditBox
  , content
  , lastOperation

    -- * Line type
  , Line(Line)
  , singleLine
  , endLine
  , HasLine(..)

  -- * Content type
  , Content
  , shift
  , above
  , below

  -- * Operations
  , delete
  , backspace
  , home
  , end
  , killHome
  , killEnd
  , killWordBackward
  , killWordForward
  , yank
  , toggle
  , left
  , right
  , leftWord
  , rightWord
  , insert
  , insertPaste
  , insertString
  , earlier
  , later
  , success
  , insertDigraph

  -- * Last operation
  , LastOperation(..)

  ) where

import           Client.State.EditBox.Content
import           Control.Lens hiding (below)
import           Data.List.NonEmpty (NonEmpty)
import           Digraphs (Digraph)
import           Data.Map (Map)
import           Data.Text (Text)


data EditBox = EditBox
  { _content       :: !Content
  , _history       :: ![NonEmpty String]
  , _historyPos    :: !Int
  , _yankBuffer    :: String
  , _lastOperation :: !LastOperation
  }
  deriving (Read, Show)

data LastOperation
  = TabOperation String
  | KillOperation
  | OtherOperation
  deriving (Read, Show)

makeLenses ''EditBox

-- | Default 'EditBox' value
defaultEditBox :: EditBox
defaultEditBox = EditBox
  { _content       = noContent
  , _history       = []
  , _historyPos    = -1
  , _yankBuffer    = ""
  , _lastOperation = OtherOperation
  }

instance HasLine EditBox where
  line = content . line

data KillDirection = KillForward | KillBackward

-- | Sets the given string to the yank buffer unless the string is empty.
updateYankBuffer :: KillDirection -> String -> EditBox -> EditBox
updateYankBuffer dir str e =
  case view lastOperation e of
    _ | null str  -> set lastOperation OtherOperation e -- failed kill interrupts kill sequence
    KillOperation ->
      case dir of
        KillForward  -> over yankBuffer (++ str) e
        KillBackward -> over yankBuffer (str ++) e
    _ -> set yankBuffer str
       $ set lastOperation KillOperation e

-- | Indicate that the contents of the text box were successfully used
-- by the program. This clears the first line of the contents and updates
-- the history.
success :: EditBox -> EditBox
success e
  = over history (cons (pure sent))
  $ set  content c
  $ set  lastOperation OtherOperation
  $ set  historyPos (-1)
  $ e
 where
 (sent, c) = shift $ view content e

replaceList :: Int -> [a] -> [a] -> [a]
replaceList i rpl xs =
  case splitAt i xs of
    (a, b) -> a ++ rpl ++ drop 1 b

-- | Update the editbox to reflect the earlier element in the history.
earlier :: EditBox -> Maybe EditBox
earlier e =
  do x <- preview (history . ix (i+1)) e
     return $ set content (fromStrings x)
            $ set lastOperation OtherOperation
            $ set historyPos i'
            $ over history updateHistory e
  where
    i = view historyPos e

    i' | i < 0     = length txt
       | otherwise = length txt + i

    txt = filter (/= pure "") [toStrings (view content e)]

    updateHistory h
      | i < 0     = txt ++ h
      | otherwise = replaceList i txt h

-- | Update the editbox to reflect the later element in the history.
later :: EditBox -> Maybe EditBox
later e
  | i < 0 && null txt = Nothing
  | otherwise = Just $!
                  set content newContent
                $ set lastOperation OtherOperation
                $ set historyPos i'
                $ over history updateHistory e
  where
    txt = filter (/= pure "") [toStrings (view content e)]

    i = view historyPos e

    i' | i < 0 = -1
       | otherwise = i - 1

    newContent = maybe noContent fromStrings
               $ preview (history . ix (i-1)) e

    updateHistory h
      | i < 0     = txt ++ h
      | otherwise = replaceList i txt h

-- | Jump the cursor to the beginning of the input.
home :: EditBox -> EditBox
home
  = set lastOperation OtherOperation
  . over content jumpLeft

-- | Jump the cursor to the end of the input.
end :: EditBox -> EditBox
end
  = set lastOperation OtherOperation
  . over content jumpRight

-- | Delete all text from the cursor to the end and store it in
-- the yank buffer.
killEnd :: EditBox -> EditBox
killEnd e
  | null kill
  = case view (content . below) e of
      []   -> e
      b:bs -> set (content . below) bs
            $ updateYankBuffer KillForward ('\n':b) e
  | otherwise
  = set line (endLine keep)
  $ updateYankBuffer KillForward kill e
  where
  Line n txt = view line e
  (keep,kill) = splitAt n txt

-- | Delete all text from the cursor to the beginning and store it in
-- the yank buffer.
killHome :: EditBox -> EditBox
killHome e
  | null kill
  = case view (content . above) e of
      []   -> e
      a:as -> set (content . above) as
            $ updateYankBuffer KillBackward (a++"\n") e

  | otherwise
  = set line (Line 0 keep)
  $ updateYankBuffer KillBackward kill e
  where
  Line n txt = view line e
  (kill,keep) = splitAt n txt

-- | Insert the yank buffer at the cursor.
yank :: EditBox -> EditBox
yank e
  = over content (insertString (view yankBuffer e))
  $ set lastOperation OtherOperation e

-- | Kill the content from the cursor back to the previous word boundary.
-- When @yank@ is set the yank buffer will be updated.
killWordBackward :: (Char -> Bool) -> Bool {- ^ yank -} -> EditBox -> EditBox
killWordBackward p saveKill e
  = sometimesUpdateYank
  $ set line (Line (length l') (l'++r))
  $ e
  where
  Line n txt = view line e
  (l,r) = splitAt n txt
  (sp,l1) = span  p (reverse l)
  (wd,l2) = break p l1
  l' = reverse l2
  yanked = reverse (sp++wd)

  sometimesUpdateYank
    | saveKill  = updateYankBuffer KillBackward yanked
    | otherwise = id -- don't update operation

-- | Kill the content from the curser forward to the next word boundary.
-- When @yank@ is set the yank buffer will be updated
killWordForward :: (Char -> Bool) -> Bool {- ^ yank -} -> EditBox -> EditBox
killWordForward p saveKill e
  = sometimesUpdateYank
  $ set line (Line (length l) (l++r2))
  $ e
  where
  Line n txt = view line e
  (l,r) = splitAt n txt
  (sp,r1) = span  p r
  (wd,r2) = break p r1
  yanked = sp++wd

  sometimesUpdateYank
    | saveKill  = updateYankBuffer KillForward yanked
    | otherwise = id -- don't update operation

-- | Insert a character at the cursor and advance the cursor.
insert :: Char -> EditBox -> EditBox
insert c
  = set lastOperation OtherOperation
  . over content (insertChar c)


insertPaste :: String -> EditBox -> EditBox
insertPaste paste
  = over content (insertPastedString paste)
  . set lastOperation OtherOperation


insertDigraph :: Map Digraph Text -> EditBox -> Maybe EditBox
insertDigraph extras
  = content (digraph extras)
  . set lastOperation OtherOperation
