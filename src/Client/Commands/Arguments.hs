{-# Language GADTs, KindSignatures #-}

{-|
Module      : Client.Configuration
Description : Client configuration format and operations
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module defines the top-level configuration information for the client.
-}

module Client.Commands.Arguments
  ( Arguments(..)
  , parseArguments
  ) where

import           Control.Monad
import           Data.Char

data Arguments :: * -> * where
  ReqTokenArg  :: String -> Arguments rest -> Arguments (String, rest)
  OptTokenArg  :: String -> Arguments rest -> Arguments (Maybe (String, rest))
  RemainingArg :: String -> Arguments String
  NoArg        :: Arguments ()

-- generateHelp :: Arguments a -> String
-- generateHelp NoArg = ""
-- generateHelp (OptTokenArg name rest) = " [" ++ name ++ generateHelp rest ++ "]"
-- generateHelp (ReqTokenArg name rest) = " " ++ name ++ generateHelp rest
-- generateHelp (RemainingArg name    ) = " " ++ name ++ "..."



parseArguments :: Arguments a -> String -> Maybe a
parseArguments arg xs =
  case arg of
    NoArg          -> guard (all isSpace xs)
    RemainingArg _ -> Just xs
    OptTokenArg _ rest
      | all isSpace xs -> Just Nothing
      | otherwise ->
          do let (tok, xs') = nextToken xs
             rest' <- parseArguments rest xs'
             return (Just (tok, rest'))
    ReqTokenArg _ rest ->
          do let (tok, xs') = nextToken xs
             guard (not (null tok))
             rest' <- parseArguments rest xs'
             return (tok, rest')

nextToken :: String -> (String, String)
nextToken = break isSpace . dropWhile isSpace
