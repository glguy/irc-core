{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Client.EditBox
Description : Console-mode text box
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides support for the text operations important for
providing a text input in the IRC client. It tracks user input
history, tab completion history, and provides many update operations
which are mapped to the keyboard in "Client.EventLoop".

-}

module Client.EditBox
  ( EditBox
  , content
  , pos
  , tabSeed
  , delete
  , backspace
  , home
  , end
  , killHome
  , killEnd
  , killWord
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

import Control.Lens
import Data.Char

data EditBox = EditBox
  { _content :: !String
  , _pos     :: !Int
  , _history :: ![String]
  , _historyPos :: !Int
  , _yankBuffer :: !(String)
  , _tabSeed :: !(Maybe String)
  }
  deriving (Read, Show)

makeLenses ''EditBox

-- | Default 'EditBox value
empty :: EditBox
empty = EditBox
  { _content = ""
  , _pos     = 0
  , _history = []
  , _historyPos = -1
  , _yankBuffer = ""
  , _tabSeed = Nothing
  }

-- | Sets the given string to the yank buffer unless the string is empty.
updateYankBuffer :: String -> EditBox -> EditBox
updateYankBuffer str
  | null str  = id
  | otherwise = set yankBuffer str

-- | Indicate that the contents of the text box were successfully used
-- by the program. This clears the contents and cursor and updates the
-- history.
success :: EditBox -> EditBox
success e
  = over history (cons (view content e))
  $ set  content ""
  $ set  tabSeed Nothing
  $ set  historyPos (-1)
  $ set  pos        0 e

-- | Update the editbox to reflect the earlier element in the history.
earlier :: EditBox -> Maybe EditBox
earlier e =
  do let i = view historyPos e + 1
     x <- preview (history . ix i) e
     return $ set content x
            $ set pos (length x)
            $ set historyPos i e

-- | Update the editbox to reflect the later element in the history.
later :: EditBox -> Maybe EditBox
later e
  | i <  0 = Nothing
  | i == 0 = Just
           $ set content ""
           $ set pos     0
           $ set historyPos (-1) e
  | otherwise =
      do x <- preview (history . ix (i-1)) e
         return $ set content x
                $ set pos (length x)
                $ set historyPos (i-1) e
  where
  i = view historyPos e

-- Remove a character without the associated checks
-- internal helper for backspace and delete
removeImpl :: EditBox -> EditBox
removeImpl e
  = set content (a++drop 1 b)
  $ set tabSeed Nothing
  $ over pos (min (views content length e - 1)) e
  where
  (a,b) = splitAt (view pos e) (view content e)

-- | Delete the character after the cursor.
delete :: EditBox -> EditBox
delete e
  | view pos e < views content length e = removeImpl e
  | otherwise = e

-- | Delete the character before the cursor.
backspace :: EditBox -> EditBox
backspace e
  | view pos e > 0 = removeImpl (left e)
  | otherwise      = e

-- | Jump the cursor to the beginning of the input.
home :: EditBox -> EditBox
home
  = set tabSeed Nothing
  . set pos 0

-- | Jump the cursor to the end of the input.
end :: EditBox -> EditBox
end e
  = set tabSeed Nothing
  $ set pos (views content length e) e

-- | Delete all text from the cursor to the end and store it in
-- the yank buffer.
killEnd :: EditBox -> EditBox
killEnd e
  = set content keep
  $ updateYankBuffer kill e
  where
  (keep,kill) = splitAt (view pos e) (view content e)

-- | Delete all text from the cursor to the beginning and store it in
-- the yank buffer.
killHome :: EditBox -> EditBox
killHome e
  = set content keep
  $ set pos 0
  $ set tabSeed Nothing
  $ updateYankBuffer kill e
  where
  (kill,keep) = splitAt (view pos e) (view content e)

-- | Insert the yank buffer at the cursor.
paste :: EditBox -> EditBox
paste e = insertString (view yankBuffer e) e

-- | Kill the content from the cursor back to the previous word boundary.
-- When @yank@ is set the yank buffer will be updated.
killWord :: Bool {- ^ yank -} -> EditBox -> EditBox
killWord yank e
  = set pos (length l')
  $ sometimesUpdateYank
  $ set content (l'++r) e
  where
  (l,r) = splitAt (view pos e) (view content e)
  (sp,l1) = span  isSpace (reverse l)
  (wd,l2) = break isSpace l1
  l' = reverse l2
  yanked = reverse (sp++wd)

  sometimesUpdateYank
    | yank = updateYankBuffer yanked
    | otherwise = id

-- | Insert a character at the cursor and advance the cursor.
insert :: Char -> EditBox -> EditBox
insert c
  = set tabSeed Nothing
  . insertString [c]

-- | Insert a string at the cursor and advance the cursor.
insertString :: String -> EditBox -> EditBox
insertString str e
  = over pos (+length str)
  $ set content (a ++ str ++ b) e
  where
  (a,b) = splitAt (view pos e) (view content e)

-- | Move the cursor left.
left :: EditBox -> EditBox
left e
  = over pos (max 0 . subtract 1) e

-- | Move the cursor right.
right :: EditBox -> EditBox
right e
  = over pos (min (views content length e) . (+1)) e

-- | Move the cursor left to the previous word boundary.
leftWord :: EditBox -> EditBox
leftWord e =
  case search of
    [] -> set pos 0 e
    (i,_):_ -> set pos (i+1) e
  where
  search = dropWhile (isAlphaNum . snd)
         $ dropWhile (not . isAlphaNum . snd)
         $ reverse
         $ take (view pos e)
         $ zip [0..]
         $ view content e

-- | Move the cursor right to the next word boundary.
rightWord :: EditBox -> EditBox
rightWord e =
  case search of
    [] -> set pos (views content length e) e
    (i,_):_ -> set pos i e
  where
  search = dropWhile (isAlphaNum . snd)
         $ dropWhile (not . isAlphaNum . snd)
         $ drop (view pos e)
         $ zip [0..]
         $ view content e
