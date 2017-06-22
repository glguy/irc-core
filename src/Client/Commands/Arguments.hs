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
  , ArgumentAdaptor(..)
  , parseArguments
  , forgetFormats
  ) where

import           Control.Monad

-- | Description of a command's arguments indexed by the result of parsing
-- those arguments. Arguments are annotated with a 'String' describing the
-- argument.
data ArgumentSpec :: * -> * -> * where

  -- | A required space-delimited token
  ReqTokenArg  :: String -> ArgumentSpec r rest -> ArgumentSpec r (String, rest)

  -- | An optional space-delimited token
  OptTokenArg  :: String -> ArgumentSpec r rest -> ArgumentSpec r (Maybe (String, rest))

  -- | Take all the remaining text in free-form
  RemainingArg :: String -> ArgumentSpec r String

  -- | No arguments
  NoArg        :: ArgumentSpec r ()

  -- | Format argument
  FormatArg :: String -> (r -> String -> Maybe (ArgumentAdaptor r a)) -> ArgumentSpec r a

data ArgumentAdaptor :: * -> * -> * where
  ArgumentAdaptor :: ArgumentSpec r a -> (a -> b) -> ArgumentAdaptor r b


forgetFormats :: ArgumentSpec r1 a -> ArgumentSpec r2 a
forgetFormats spec =
  case spec of
    ReqTokenArg s rest -> ReqTokenArg s (forgetFormats rest)
    OptTokenArg s rest -> OptTokenArg s (forgetFormats rest)
    RemainingArg s     -> RemainingArg s
    NoArg              -> NoArg
    FormatArg s _      -> FormatArg s (\_ _ -> Nothing)


instance Show (ArgumentSpec r s) where
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
      FormatArg s _      -> showParen (p >= 11)
                          $ showString "FormatArg " . showsPrec 11 s



-- | Parse the given input string using an argument specification.
-- The arguments should start with a space but might have more.
parseArguments ::
  ArgumentSpec r a {- ^ specification -} ->
  r                {- ^ environment   -} ->
  String           {- ^ input string  -} ->
  Maybe a          {- ^ parse results -}
parseArguments arg env xs =
  case arg of
    NoArg          -> guard (all (==' ') xs)
    RemainingArg _ -> Just (drop 1 xs) -- drop the leading space
    OptTokenArg _ rest ->
      do let (tok, xs') = nextToken xs
         if null tok
           then Just Nothing
           else do rest' <- parseArguments rest env xs'
                   return (Just (tok, rest'))
    ReqTokenArg _ rest ->
      do let (tok, xs') = nextToken xs
         guard (not (null tok))
         rest' <- parseArguments rest env xs'
         return (tok, rest')
    FormatArg _ f ->
      do let (tok, xs') = nextToken xs
         ArgumentAdaptor rest adapt <- f env tok
         rest' <- parseArguments rest env xs'
         return (adapt rest')

-- | Return the next space delimited token. Leading space is dropped.
nextToken :: String -> (String, String)
nextToken = break (==' ') . dropWhile (==' ')
