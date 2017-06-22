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
  ) where

import           Client.Commands.Arguments
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Control.Lens
import           Data.Semigroup
import qualified Data.Text as Text
import           Graphics.Vty.Attributes

-- | Parse command arguments against a given 'ArgumentSpec'.
-- The given text will be rendered and then any missing arguments
-- will be indicated by extra placeholder values appended onto the
-- image. Parameters are rendered with 'plainText' except for
-- the case of 'RemainingArg' which supports WYSIWYG.
argumentsImage ::
  r                {- ^ client state           -} ->
  Palette          {- ^ palette                -} ->
  ArgumentSpec r a {- ^ argument specification -} ->
  Bool             {- ^ render placeholders    -} ->
  String           {- ^ input text             -} ->
  Image'           {- ^ rendered text          -}
argumentsImage st pal spec pl xs
  | all (==' ') xs = placeholders
                  <> string defAttr (drop (imageWidth placeholders) xs)
  | otherwise =
     case spec of
       NoArg           -> plainText xs
       ReqTokenArg _ a -> plainText token <> argumentsImage st pal a pl xs'
       OptTokenArg _ a -> plainText token <> argumentsImage st pal a pl xs'
       RemainingArg _  -> parseIrcText' True (Text.pack xs)
       FormatArg _ k   -> plainText token <>
                          case k st token2 of
                            Nothing -> mempty
                            Just (ArgumentAdaptor a _) ->
                              argumentsImage st pal a pl xs'

  where
    token = token1 ++ token2
    (token1,(token2,xs')) =
         break (==' ') <$> span (==' ') xs

    placeholders
      | pl        = mkPlaceholders pal spec
      | otherwise = mempty

-- | Construct an 'Image' containing placeholders for each
-- of the remaining arguments.
mkPlaceholders :: Palette -> ArgumentSpec r a -> Image'
mkPlaceholders pal arg =
  case arg of
    NoArg           -> mempty
    ReqTokenArg n a -> leader
                    <> string (view palCommandPlaceholder pal) n
                    <> mkPlaceholders pal a
    OptTokenArg n a -> leader
                    <> string (view palCommandPlaceholder pal) (n ++ "?")
                    <> mkPlaceholders pal a
    RemainingArg n  -> leader
                    <> string (view palCommandPlaceholder pal) (n ++ "…")
    FormatArg n _   -> leader
                    <> string (view palCommandPlaceholder pal) n
  where
    leader = char defAttr ' '
