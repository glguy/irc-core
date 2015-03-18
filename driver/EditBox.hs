{-# LANGUAGE TemplateHaskell #-}
module EditBox
  ( EditBox
  , content
  , pos
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
  , _history :: [String]
  , _historyPos :: !Int
  , _yankBuffer :: String
  }
  deriving (Read, Show)

makeLenses ''EditBox

empty :: EditBox
empty = EditBox
  { _content = ""
  , _pos     = 0
  , _history = []
  , _historyPos = -1
  , _yankBuffer = ""
  }

updateYankBuffer :: String -> EditBox -> EditBox
updateYankBuffer str
  | null str  = id
  | otherwise = set yankBuffer str

success :: EditBox -> EditBox
success e
  = over history (cons (view content e))
  $ set  content ""
  $ set  historyPos (-1)
  $ set  pos        0 e

earlier :: EditBox -> Maybe EditBox
earlier e =
  do let i = view historyPos e + 1
     x <- preview (history . ix i) e
     return $ set content x
            $ set pos (length x)
            $ set historyPos i e

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
  $ over pos (min (views content length e - 1)) e
  where
  (a,b) = splitAt (view pos e) (view content e)

delete :: EditBox -> EditBox
delete e
  | view pos e < views content length e = removeImpl e
  | otherwise = e

backspace :: EditBox -> EditBox
backspace e
  | view pos e > 0 = removeImpl (left e)
  | otherwise      = e

home :: EditBox -> EditBox
home e
  = set pos 0 e

end :: EditBox -> EditBox
end e
  = set pos (views content length e) e

killEnd :: EditBox -> EditBox
killEnd e
  = set content keep
  $ updateYankBuffer kill e
  where
  (keep,kill) = splitAt (view pos e) (view content e)

killHome :: EditBox -> EditBox
killHome e
  = set content keep
  $ set pos 0
  $ updateYankBuffer kill e
  where
  (kill,keep) = splitAt (view pos e) (view content e)

paste :: EditBox -> EditBox
paste e = insertString (view yankBuffer e) e

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

insert :: Char -> EditBox -> EditBox
insert c
  = insertString [c]

insertString :: String -> EditBox -> EditBox
insertString str e
  = over pos (+length str)
  $ set content (a ++ str ++ b) e
  where
  (a,b) = splitAt (view pos e) (view content e)

left :: EditBox -> EditBox
left e
  = over pos (max 0 . subtract 1) e

right :: EditBox -> EditBox
right e
  = over pos (min (views content length e) . (+1)) e

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
