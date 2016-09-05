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
  ( ArgumentSpec(..)
  , parseArguments
  ) where

import           Control.Monad
import           Data.Attoparsec.Text as P
import           Data.Char


-- | Description of a command's arguments indexed by the result of parsing
-- those arguments. Arguments are annotated with a 'String' describing the
-- argument.
data ArgumentSpec :: * -> * where

  -- | A required space-delimited token
  ReqTokenArg  :: String -> ArgumentSpec rest -> ArgumentSpec (String, rest)

  -- | An optional space-delimited token
  OptTokenArg  :: String -> ArgumentSpec rest -> ArgumentSpec (Maybe (String, rest))

  -- | Take all the remaining text in free-form
  RemainingArg :: String -> ArgumentSpec String

  -- | No arguments
  NoArg        :: ArgumentSpec ()

instance Show (ArgumentSpec s) where
  showsPrec p spec =
    case spec of
      ReqTokenArg s rest -> showParen (p >= 11)
                          $ showString "ReqTokenArg "
                          . showsPrec 11 s . showChar ' ' . showsPrec 11 rest
      OptTokenArg s rest -> showParen (p >= 11)
                          $ showString "OptTokenArg "
                          . showsPrec 11 s . showChar ' ' . showsPrec 11 rest
      RemainingArg s     -> showParen (p >= 11)
                          $ showString "RemainingArg " . showsPrec 11 s
      NoArg              -> showString "NoArg"

-- | Parse the given input string using an argument specification.
-- The arguments should start with a space but might have more.
parseArguments ::
  ArgumentSpec a {- ^ specification -} ->
  String         {- ^ input string  -} ->
  Maybe a        {- ^ parse results -}
parseArguments arg xs =
  case arg of
    NoArg          -> guard (all (==' ') xs)
    RemainingArg _ -> Just (drop 1 xs) -- drop the leading space
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
