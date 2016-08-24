{-|
Module      : View
Description : Local definition of view
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

-}
module View (view) where

import Data.Functor.Const

view l x = getConst (l Const x)
