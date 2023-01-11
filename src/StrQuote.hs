{-|
Module      : StrQuote
Description : Template Haskell quasi-quoter for string literals
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module StrQuote (str) where

import Language.Haskell.TH (Pat(LitP), Type(LitT), stringE, Lit(StringL), TyLit(StrTyLit))
import Language.Haskell.TH.Quote (QuasiQuoter(..))

str :: QuasiQuoter
str = QuasiQuoter
  { quoteExp  = stringE
  , quotePat  = pure . LitP . StringL
  , quoteType = pure . LitT . StrTyLit
  , quoteDec  = const (fail "str is for expressions")
  }
