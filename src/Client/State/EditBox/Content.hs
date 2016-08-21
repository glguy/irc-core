{-# Language TemplateHaskell #-}
{-|
Module      : Client.State.EditBox.Content
Description : Multiline text container with cursor
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Client.State.EditBox.Content where

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
shift (Content [] l (b:bs)) = (view text l, Content [] (Line 0 b) bs)

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
              . set  current (Line 0 b)
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
