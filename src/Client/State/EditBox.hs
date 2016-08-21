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
  ( Line(Line)
  , endLine
  , HasLine(..)
  , Content
  , above
  , below
  , singleLine
  , firstLine
  , EditBox
  , content
  , tabSeed
  , delete
  , backspace
  , home
  , end
  , killHome
  , killEnd
  , killWordBackward
  , killWordForward
  , paste
  , left
  , right
  , leftWord
  , rightWord
  , insert
  , insertString
  , empty
  , earlier
  , later
  , success
  ) where

import           Client.State.EditBox.Content
import           Control.Lens hiding (below)
import           Data.Char


data EditBox = EditBox
  { _content :: !Content
  , _history :: ![String]
  , _historyPos :: !Int
  , _yankBuffer :: !(String)
  , _tabSeed :: !(Maybe String)
  }
  deriving (Read, Show)

makeLenses ''EditBox

-- | Default 'EditBox' value
empty :: EditBox
empty = EditBox
  { _content = noContent
  , _history = []
  , _historyPos = -1
  , _yankBuffer = ""
  , _tabSeed = Nothing
  }

instance HasLine EditBox where
  line = content . line

-- | Sets the given string to the yank buffer unless the string is empty.
updateYankBuffer :: String -> EditBox -> EditBox
updateYankBuffer str
  | null str  = id
  | otherwise = set yankBuffer str

-- | Indicate that the contents of the text box were successfully used
-- by the program. This clears the first line of the contents and updates
-- the history.
success :: EditBox -> EditBox
success e
  = over history (cons sent)
  $ set  content c
  $ set  tabSeed Nothing
  $ set  historyPos (-1)
  $ e
 where
 (sent, c) = shift $ view content e

-- | Update the editbox to reflect the earlier element in the history.
earlier :: EditBox -> Maybe EditBox
earlier e =
  do let i = view historyPos e + 1
     x <- preview (history . ix i) e
     return $ set content (singleLine . endLine $ x)
            $ set historyPos i e

-- | Update the editbox to reflect the later element in the history.
later :: EditBox -> Maybe EditBox
later e
  | i <  0 = Nothing
  | i == 0 = Just
           $ set content noContent
           $ set historyPos (-1) e
  | otherwise =
      do x <- preview (history . ix (i-1)) e
         return $ set content (singleLine . endLine $ x)
                $ set historyPos (i-1) e
  where
  i = view historyPos e

-- | Jump the cursor to the beginning of the input.
home :: EditBox -> EditBox
home
  = set tabSeed Nothing
  . over content jumpLeft

-- | Jump the cursor to the end of the input.
end :: EditBox -> EditBox
end
  = set tabSeed Nothing
  . over content jumpRight

-- | Delete all text from the cursor to the end and store it in
-- the yank buffer.
killEnd :: EditBox -> EditBox
killEnd e
  | null kill
  = case view (content.below) e of
      []   -> e
      b:bs -> set (content.below) bs
            $ updateYankBuffer b e
  | otherwise
  = set line (endLine keep)
  $ updateYankBuffer kill e
  where
  Line n txt = view line e
  (keep,kill) = splitAt n txt

-- | Delete all text from the cursor to the beginning and store it in
-- the yank buffer.
killHome :: EditBox -> EditBox
killHome e
  | null kill
  = case view (content.above) e of
      []   -> e
      a:as -> set (content.above) as
            . set tabSeed Nothing
            $ updateYankBuffer a e

  | otherwise
  = set line (Line 0 keep)
  $ set tabSeed Nothing
  $ updateYankBuffer kill e
  where
  Line n txt = view line e
  (kill,keep) = splitAt n txt

-- | Insert the yank buffer at the cursor.
paste :: EditBox -> EditBox
paste e = over content (insertString (view yankBuffer e)) e

-- | Kill the content from the cursor back to the previous word boundary.
-- When @yank@ is set the yank buffer will be updated.
killWordBackward :: Bool {- ^ yank -} -> EditBox -> EditBox
killWordBackward yank e
  = sometimesUpdateYank
  $ set line (Line (length l') (l'++r))
  $ e
  where
  Line n txt = view line e
  (l,r) = splitAt n txt
  (sp,l1) = span  isSpace (reverse l)
  (wd,l2) = break isSpace l1
  l' = reverse l2
  yanked = reverse (sp++wd)

  sometimesUpdateYank
    | yank = updateYankBuffer yanked
    | otherwise = id

-- | Kill the content from the curser forward to the next word boundary.
-- When @yank@ is set the yank buffer will be updated
killWordForward :: Bool {- ^ yank -} -> EditBox -> EditBox
killWordForward yank e
  = sometimesUpdateYank
  $ set line (Line (length l) (l++r2))
  $ e
  where
  Line n txt = view line e
  (l,r) = splitAt n txt
  (sp,r1) = span  isSpace r
  (wd,r2) = break isSpace r1
  yanked = sp++wd

  sometimesUpdateYank
    | yank = updateYankBuffer yanked
    | otherwise = id

-- | Insert a character at the cursor and advance the cursor.
insert :: Char -> EditBox -> EditBox
insert c
  = set tabSeed Nothing
  . over content (insertString [c])
