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
  , left
  , right
  , insert
  , insertString
  , empty
  ) where

import Control.Lens
import Data.Char

data EditBox = EditBox
  { _content :: !String
  , _pos     :: !Int
  }
  deriving (Read, Show)

makeLenses ''EditBox

empty :: EditBox
empty = EditBox
  { _content = ""
  , _pos     = 0
  }

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
  = over content (take (view pos e)) e

killHome :: EditBox -> EditBox
killHome e
  = home
  $ over content (drop (view pos e)) e

killWord :: EditBox -> EditBox
killWord e
  = set pos (length a')
  $ set content (a'++b) e
  where
  (a,b) = splitAt (view pos e) (view content e)
  a' = reverse
     $ dropWhile (not . isSpace)
     $ dropWhile isSpace
     $ reverse a

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
