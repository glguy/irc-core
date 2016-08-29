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
  , argumentsImage
  , plainText
  ) where

import           Client.Image.MircFormatting
import           Control.Monad
import           Data.Char
import qualified Data.Text as Text
import           Graphics.Vty.Image

data Arguments :: * -> * where
  ReqTokenArg  :: String -> Arguments rest -> Arguments (String, rest)
  OptTokenArg  :: String -> Arguments rest -> Arguments (Maybe (String, rest))
  RemainingArg :: String -> Arguments String
  NoArg        :: Arguments ()

plainText :: String -> Image
plainText "" = emptyImage
plainText xs =
  case break isControl xs of
    (first, ""       ) -> string defAttr first
    (first, cntl:rest) -> string defAttr first <|>
                          controlImage cntl <|>
                          plainText rest

argumentsImage :: Arguments a -> String -> Image
argumentsImage = argumentsImage' True

argumentsImage' :: Bool -> Arguments a -> String -> Image
argumentsImage' isFirst arg xs
  | all isSpace xs =
      placeholder <|> string defAttr (drop (imageWidth placeholder) xs)
  | otherwise =
     case arg of
       NoArg           -> plainText xs
       ReqTokenArg _ a -> plainText token <|> argumentsImage' False a xs'
       OptTokenArg _ a -> plainText token <|> argumentsImage' False a xs'
       RemainingArg _  -> parseIrcTextExplicit (Text.pack xs)

  where
    token = token1 ++ token2
    (token1,(token2,xs')) =
         break isSpace <$> span isSpace xs

    placeholder = mkPlaceholder isFirst arg

mkPlaceholder :: Bool -> Arguments a -> Image
mkPlaceholder isFirst arg =
  case arg of
    NoArg           -> emptyImage
    ReqTokenArg n a -> leader <|> string attr n <|> mkPlaceholder False a
    OptTokenArg n a -> leader <|> string attr n <|> mkPlaceholder False a
    RemainingArg n  -> leader <|> string attr n
  where
    attr = withStyle defAttr reverseVideo
    leader | isFirst = emptyImage
           | otherwise = char defAttr ' '


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
