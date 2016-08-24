{-|
Module      : View
Description : Local definition of view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}
module View (view) where

import Data.Functor.Const

-- | Local definition of lens package's view.
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l x = getConst (l Const x)
