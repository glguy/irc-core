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

beginLine :: String -> Line
beginLine = Line 0

endLine :: String -> Line
endLine s = Line (length s) s

-- | Zipper-ish view of the multi-line content of an 'EditBox'.
-- Lines '_above' the '_currentLine' are stored in reverse order.
data Content = Content
  { _above       :: ![String]
  , _currentLine :: !Line
  , _below       :: ![String]
  }
  deriving (Read, Show)

makeLenses ''Content

instance HasLine Content where
  line = currentLine

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
shift (Content [] l (b:bs)) = (view text l, Content [] (beginLine b) bs)

jumpLeft :: Content -> Content
jumpLeft c
  | view pos c == 0
  , a:as <- view above c
  = over below (view text c :)
  . set text a
  . set above as
  $ c
  | otherwise = set pos 0 c

jumpRight :: Content -> Content
jumpRight c
  | view pos c == len
  , b:bs <- view below c
  = over above (view text c :)
  . set line (endLine b)
  . set below bs
  $ c
  | otherwise = set pos len c
 where len = views text length c

-- Move the cursor left, across lines if necessary.
left :: Content -> Content
left c =
  let Line n s = view line c in
  case compare n 0 of
    GT                        -> (pos -~ 1) c
    EQ | a:as <- view above c -> over below (cons s)
                               . set above as
                               . set line (endLine a)
                               $ c
    _                         -> c

-- Move the cursor right, across lines if necessary.
right :: Content -> Content
right c =
  let Line n s = view line c in
  case compare n (length s) of
    LT                        -> (pos +~ 1) c
    EQ | b:bs <- view below c -> over above (cons s)
                               . set below bs
                               . set line (beginLine b)
                               $ c

    _                         -> c

-- | Move the cursor left to the previous word boundary.
leftWord :: Content -> Content
leftWord c
  | n == 0
  = case view above c of
      []     -> c
      (a:as) -> leftWord
              . set  line  (endLine a)
              . over below (cons txt)
              . set  above as
              $ c
  | otherwise
  = case search of
      []      -> set pos 0     c
      (i,_):_ -> set pos (i+1) c
  where
  Line n txt = view line c
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
              . set  line  (beginLine b)
              . over above (cons txt)
              . set  below bs
              $ c
  | otherwise
  = case search of
      []      -> set pos (length txt) c
      (i,_):_ -> set pos i c
  where
  Line n txt = view line c
  search = dropWhile (isAlphaNum . snd)
         $ dropWhile (not . isAlphaNum . snd)
         $ drop n
         $ zip [0..] txt

-- | Delete the character before the cursor.
backspace :: Content -> Content
backspace c
  | n == 0
  = case view above c of
      []   -> c
      a:as -> set above as
            . set line (Line (length a) (a ++ s))
            $ c

  | (preS, postS) <- splitAt (n-1) s
  = set line (Line (n-1) (preS ++ drop 1 postS)) c
  where
    Line n s = view line c

-- | Delete the character after/under the cursor.
delete :: Content -> Content
delete c =
  let Line n s = view line c in
  case splitAt n s of
    (preS, _:postS) -> set text (preS ++ postS) c
    _               -> case view below c of
                         []   -> c
                         b:bs -> set below bs
                               . set text (s ++ b)
                               $ c

insertString :: String -> Content -> Content
insertString ins c =
  case push (view above c) (preS ++ l) ls of
    (newAbove, newLine) -> set above newAbove
                         $ set line newLine c
  where
    l:ls          = lines (ins ++ "\n")
    Line n txt    = view line c
    (preS, postS) = splitAt n txt

    push stk x []     = (stk, Line (length x) (x ++ postS))
    push stk x (y:ys) = push (x:stk) y ys
