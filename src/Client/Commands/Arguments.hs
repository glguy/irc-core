{-# Language GADTs, KindSignatures #-}

{-|
Module      : Client.Commands.Arguments
Description : Command argument description and parsing
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a description for the arguments expected
by command commands as well as a way to parse those arguments.
-}

module Client.Commands.Arguments
  ( Arguments(..)
  , parseArguments
  ) where

import           Control.Monad

-- | Description of a command's arguments indexed by the result of parsing
-- those arguments. Arguments are annotated with a 'String' describing the
-- argument.
data Arguments :: * -> * where

  -- | A required space-delimited token
  ReqTokenArg  :: String -> Arguments rest -> Arguments (String, rest)

  -- | An optional space-delimited token
  OptTokenArg  :: String -> Arguments rest -> Arguments (Maybe (String, rest))

  -- | Take all the remaining text in free-form
  RemainingArg :: String -> Arguments String

  -- | No arguments
  NoArg        :: Arguments ()


-- | Parse the given input string using an argument specification.
parseArguments ::
  Arguments a {- ^ specification -} ->
  String      {- ^ input string  -} ->
  Maybe a     {- ^ parse results -}
parseArguments arg xs =
  case arg of
    NoArg          -> guard (all (==' ') xs)
    RemainingArg _ -> Just xs
    OptTokenArg _ rest ->
      do let (tok, xs') = nextToken xs
         if null tok
           then Just Nothing
           else do rest' <- parseArguments rest xs'
                   return (Just (tok, rest'))
    ReqTokenArg _ rest ->
      do let (tok, xs') = nextToken xs
         guard (not (null tok))
         rest' <- parseArguments rest xs'
         return (tok, rest')

-- | Return the next space delimited token. Leading space is dropped.
nextToken :: String -> (String, String)
nextToken = break (==' ') . dropWhile (==' ')
