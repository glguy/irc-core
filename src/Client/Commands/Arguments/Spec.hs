{-# Language KindSignatures, GADTs #-}

{-|
Module      : Client.Commands.Arguments.Spec
Description : Argument specifications used within client commands
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Client.Commands.Arguments.Spec
  ( Args
  , simpleToken
  , remainingArg
  , optionalArg
  , tokenList
  , numberArg
  , optionalNumberArg
  , extensionArg
  , tokenArg
  , mapArgEnv

  , ArgumentShape(..)
  , Arg(..)
  ) where

import Control.Applicative (liftA2)
import Control.Applicative.Free (Ap, liftAp)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

type Args r = Ap (Arg r)

data ArgumentShape = TokenArgument | RemainingArgument

data Arg :: Type -> Type -> Type where
  Argument  :: ArgumentShape -> String -> (r -> String -> Maybe a) -> Arg r a
  Optional  :: Args r a -> Arg r (Maybe a)
  Extension :: String -> (r -> String -> Maybe (Args r a)) -> Arg r a
  MapEnv    :: (r -> s) -> Args s a -> Arg r a

tokenArg :: String -> (r -> String -> Maybe a) -> Args r a
tokenArg name parser = liftAp (Argument TokenArgument name parser)

remainingArg :: String -> Args r String
remainingArg name = liftAp (Argument RemainingArgument name (\_ -> Just))

optionalArg :: Args r a -> Args r (Maybe a)
optionalArg = liftAp . Optional

extensionArg :: String -> (r -> String -> Maybe (Args r a)) -> Args r a
extensionArg name parser = liftAp (Extension name parser)

simpleToken :: String -> Args r String
simpleToken name = tokenArg name (\_ -> Just)

numberArg :: Args r Int
numberArg = tokenArg "number" (\_ -> readMaybe)

optionalNumberArg :: Args r (Maybe Int)
optionalNumberArg = optionalArg (tokenArg "[number]" (\_ -> readMaybe))

mapArgEnv :: (r -> s) -> Args s a -> Args r a
mapArgEnv f = liftAp . MapEnv f

tokenList ::
  [String] {- ^ required names -} ->
  [String] {- ^ optional names -} ->
  Args r [String]
tokenList req opt = foldr addReq (foldr addOpt (pure []) opt) req
  where
    addReq name      = liftA2 (:) (simpleToken name)
    addOpt name rest = fromMaybe [] <$> optionalArg (addReq name rest)
