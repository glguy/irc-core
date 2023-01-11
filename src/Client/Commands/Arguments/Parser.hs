{-# Language GADTs #-}

{-|
Module      : Client.Commands.Arguments.Parser
Description : Interpret argument specifications as a parser
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.Commands.Arguments.Parser (parse) where

import Client.Commands.Arguments.Spec (Arg(..), Args, ArgumentShape(..))
import Control.Applicative (optional)
import Control.Applicative.Free (runAp)
import Control.Monad (guard)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State (StateT(runStateT), get, put)

------------------------------------------------------------------------
-- Parser

parse :: r -> Args r a -> String -> Maybe a
parse env spec str =
  do (a,rest) <- runStateT (parseArgs env spec) str
     guard (all (' '==) rest)
     return a

type Parser = StateT String Maybe

parseArgs :: r -> Args r a -> Parser a
parseArgs env = runAp (parseArg env)

parseArg :: r -> Arg r a -> Parser a
parseArg env spec =
  case spec of
    Argument shape _ f ->
      do t <- argumentString shape
         lift (f env t)
    Optional subspec -> optional (parseArgs env subspec)
    Extension _ parseFormat ->
      do t <- token
         subspec <- lift (parseFormat env t)
         parseArgs env subspec

argumentString :: ArgumentShape -> Parser String
argumentString TokenArgument     = token
argumentString RemainingArgument = remaining

remaining :: Parser String
remaining =
  do xs <- get
     put ""
     return $! case xs of
                 ' ':xs' -> xs'
                 _       -> xs

token :: Parser String
token =
  do xs <- get
     let (t, xs') = break (' '==) (dropWhile (' '==) xs)
     guard (not (null t))
     put xs'
     return t
