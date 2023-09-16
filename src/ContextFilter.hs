module ContextFilter (filterContext) where

-- | Like 'filter' except you can request the elements before
-- and after the matched elements.
--
-- >>> filterContext 2 1 (\x -> 4<=x&&x<=6) [0..10::Int]
-- [2,3,4,5,6,7]
--
-- >>> filterContext 2 1 even [0..10::Int]
-- [0,1,2,3,4,5,6,7,8,9,10]
--
-- >>> filterContext 2 1 (==10) [0..10::Int]
-- [8,9,10]
--
-- >>> filterContext 2 1 (==0) [0..10::Int]
-- [0,1]
--
-- >>> filterContext 0 0 (==0) [0..10::Int]
-- [0]
--
-- >>> filterContext 2 1 (==5) [0..10::Int]
-- [3,4,5,6]
--
-- >>> filterContext 1 2 (==5) [0..10::Int]
-- [4,5,6,7]
filterContext ::
  Int         {- ^ context before       -} ->
  Int         {- ^ context after        -} ->
  (a -> Bool) {- ^ predicate            -} ->
  [a]         {- ^ inputs               -} ->
  [a]         {- ^ matches with context -}
filterContext before after p xs0
  | before < 0 = error "filterContext: bad before"
  | after  < 0 = error "filterContext: bad after"
  | otherwise  = go 0 0 xs0 xs0
  where
    width = before + after

    -- i: index
    -- m: current match window
    -- xs: list to match
    -- ys: offset list to generate results from
    go i m (x:xs) yys@(y:ys) =
      if (m > 0 || px) && i >= before then y : rest else rest
      where
        rest = go (i+1) m' xs ys'
        px = p x
        m' = max (m-1) (if px then width else 0)
        ys' = if i >= before then ys else yys

    -- no more matches, so just return the remaining m window
    go _ m _ ys = take m ys
