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
  , current
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

import           Control.Lens hiding (below)
import           Data.Char

data Line = Line
  { _pos  :: !Int
  , _text :: !String
  }
  deriving (Read, Show)

makeClassy ''Line

emptyLine :: Line
emptyLine = Line 0 ""

endLine :: String -> Line
endLine s = Line (length s) s

-- | Zipper-ish view of the multi-line content of an 'EditBox'.
-- Lines 'above' the 'current' are stored in reverse order.
data Content = Content
  { _above   :: ![String]
  , _current :: !Line
  , _below   :: ![String]
  }
  deriving (Read, Show)

makeLenses ''Content

instance HasLine Content where
  line = current

-- | Default 'Content' value
noContent :: Content
noContent = Content [] emptyLine []

-- | Single line 'Content'.
singleLine :: Line -> Content
singleLine l = Content [] l []

-- | Shifts the first line off of the 'Content', yielding the
-- text of the line and the rest of the content.
shift :: Content -> (String, Content)
shift (Content [] l []) = (view text l, noContent)
shift (Content a@(_:_) l b) = (last a, Content (init a) l b)
shift (Content [] l (b:bs)) = (view text l, Content [] (endLine b) bs)

firstLine :: Content -> String
firstLine (Content a c _) = head (reverse a ++ [_text c])

jumpLeft :: Content -> Content
jumpLeft c
  | view pos c == 0
  , a:as <- view above c
  = over below (cons $ view text c)
  . set text a
  . set above as
  $ c
  | otherwise = set pos 0 c

jumpRight :: Content -> Content
jumpRight c
  | view pos c == len
  , b:bs <- view below c
  = over above (cons $ view text c)
  . set text b
  . set pos len
  . set below bs
  $ c
  | otherwise = set pos len c
 where len = views text length c

-- Move the cursor left, across lines if necessary.
left :: Content -> Content
left c
  | n > 0
  = over (current.pos) (subtract 1) c

  | n == 0
  , a:as <- view above c
  = over below (cons s)
  . set above as
  . set current (endLine a)
  $ c

  | otherwise = c
 where Line n s = view current c

-- Move the cursor right, across lines if necessary.
right :: Content -> Content
right c
  | n < length s
  = over (current.pos) (+1) c

  | n == length s
  , b:bs <- view below c
  = over above (cons s)
  . set below bs
  . set current (Line 0 b)
  $ c

  | otherwise = c
 where Line n s = view current c

-- | Move the cursor left to the previous word boundary.
leftWord :: Content -> Content
leftWord c
  | n == 0
  = case view above c of
      []     -> c
      (a:as) -> leftWord
              . set  current (endLine a)
              . over below (cons txt)
              . set  above as
              $ c
  | otherwise
  = case search of
      []      -> set (current.pos) 0     c
      (i,_):_ -> set (current.pos) (i+1) c
  where
  Line n txt = view current c
  search = dropWhile (isAlphaNum . snd)
         $ dropWhile (not . isAlphaNum . snd)
         $ reverse
         $ take n
         $ zip [0..]
         $ txt

-- | Move the cursor right to the next word boundary.
rightWord :: Content -> Content
rightWord c
  | n == length txt
  = case view below c of
      [] -> c
      (b:bs) -> rightWord
              . set  current (endLine b)
              . over above (cons txt)
              . set  below bs
              $ c
  | otherwise
  = case search of
      [] -> set (current.pos) (length txt) c
      (i,_):_ -> set (current.pos) i c
  where
  Line n txt = view current c
  search = dropWhile (isAlphaNum . snd)
         $ dropWhile (not . isAlphaNum . snd)
         $ drop n
         $ zip [0..]
         $ txt

-- | Delete the character before the cursor.
backspace :: Content -> Content
backspace c
  | n == 0
  = case view above c of
      []   -> c
      a:as -> set above as
            . set current (Line (length a) (a ++ s))
            $ c

  | (preS, postS) <- splitAt (n-1) s
  = set current (Line (n-1) (preS ++ drop 1 postS)) c
 where
 Line n s = view current c

-- | Delete the character after/under the cursor.
delete :: Content -> Content
delete c
  | n == length s
  = case view below c of
      []   -> c
      b:bs -> set below bs
            . set current (Line n (s ++ b))
            $ c

  | (preS, postS) <- splitAt n s
  = set current (Line n (preS ++ drop 1 postS)) c
 where
 Line n s = view current c

insertString :: String -> Content -> Content
insertString ins c = case push (view above c) (preS ++ l) ls of
  (newAbove, newCurrent) -> set above newAbove . set current newCurrent $ c
 where
 l:ls = lines (ins ++ "\n")
 Line n txt = view current c
 (preS, postS) = splitAt n txt

 push stk x []     = (stk, Line (length x) (x ++ postS))
 push stk x (y:ys) = push (x:stk) y ys

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
  = set (content.current) (endLine keep)
  $ updateYankBuffer kill e
  where
  Line n txt = view (content.current) e
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
  = set (content.current) (Line 0 keep)
  $ set tabSeed Nothing
  $ updateYankBuffer kill e
  where
  Line n txt = view (content.current) e
  (kill,keep) = splitAt n txt

-- | Insert the yank buffer at the cursor.
paste :: EditBox -> EditBox
paste e = over content (insertString (view yankBuffer e)) e

-- | Kill the content from the cursor back to the previous word boundary.
-- When @yank@ is set the yank buffer will be updated.
killWordBackward :: Bool {- ^ yank -} -> EditBox -> EditBox
killWordBackward yank e
  = sometimesUpdateYank
  $ set (content.current) (Line (length l') (l'++r))
  $ e
  where
  Line n txt = view (content.current) e
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
  $ set (content.current) (Line (length l) (l++r2))
  $ e
  where
  Line n txt = view (content.current) e
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
