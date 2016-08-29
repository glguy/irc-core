{-# Language GADTs #-}

{-|
Module      : Client.Image.Arguments
Description : Rendering logic for commanad arguments
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides image rendering for the textbox in the
context of command argument processing.
-}

module Client.Image.Arguments
  ( argumentsImage
  , plainText
  ) where

import           Client.Commands.Arguments
import           Client.Image.MircFormatting
import           Client.Image.Palette
import           Control.Lens
import           Data.Char
import qualified Data.Text as Text
import           Graphics.Vty.Image

plainText :: String -> Image
plainText "" = emptyImage
plainText xs =
  case break isControl xs of
    (first, ""       ) -> string defAttr first
    (first, cntl:rest) -> string defAttr first <|>
                          controlImage cntl <|>
                          plainText rest

argumentsImage :: Palette -> Arguments a -> String -> Image
argumentsImage pal arg xs
  | all (==' ') xs =
      placeholder <|> string defAttr (drop (imageWidth placeholder) xs)
  | otherwise =
     case arg of
       NoArg           -> plainText xs
       ReqTokenArg _ a -> plainText token <|> argumentsImage pal a xs'
       OptTokenArg _ a -> plainText token <|> argumentsImage pal a xs'
       RemainingArg _  -> parseIrcTextExplicit (Text.pack xs)

  where
    token = token1 ++ token2
    (token1,(token2,xs')) =
         break (==' ') <$> span (==' ') xs

    placeholder = mkPlaceholder pal arg

mkPlaceholder :: Palette -> Arguments a -> Image
mkPlaceholder pal arg =
  case arg of
    NoArg           -> emptyImage
    ReqTokenArg n a -> leader
                   <|> string (view palCommandRequired pal) n
                   <|> mkPlaceholder pal a
    OptTokenArg n a -> leader
                   <|> string (view palCommandOptional pal) n
                   <|> mkPlaceholder pal a
    RemainingArg n  -> leader
                   <|> string (view palCommandRemaining pal) n
  where
    leader = char defAttr ' '
