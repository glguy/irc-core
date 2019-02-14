module ContextFilter (filterContext) where

filterContext ::
  Int         {- ^ context before       -} ->
  Int         {- ^ context after        -} ->
  (a -> Bool) {- ^ predicate            -} ->
  [a]         {- ^ inputs               -} ->
  [a]         {- ^ matches with context -}
filterContext before after p xs
  | before < 0 = error "filterContext: bad before"
  | after  < 0 = error "filterContext: bad after"
  | otherwise  = selectList (dropSelection before selects) xs
  where
    width = before + after

    selects = go 0 xs

    go n []       = replicateKeep n
    go n (y : ys)
      | p y       = Keep (go width ys)
      | n > 0     = Keep (go (n - 1) ys)
      | otherwise = Skip (go n ys)

data Selection = End | Keep Selection | Skip Selection
  deriving (Show)

replicateKeep :: Int -> Selection
replicateKeep 0 = End
replicateKeep i = Keep (replicateKeep (i - 1))

dropSelection :: Int -> Selection -> Selection
dropSelection 0 x        = x
dropSelection _ End      = End
dropSelection i (Keep x) = dropSelection (i - 1) x
dropSelection i (Skip x) = dropSelection (i - 1) x

selectList :: Selection -> [a] -> [a]
selectList (Keep x) (y : ys) = y : selectList x ys
selectList (Skip x) (_ : ys) =     selectList x ys
selectList _        _        = []
