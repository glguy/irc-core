{-# Language BangPatterns #-}

{-|
Module      : Client.Image.Textbox
Description : Textbox renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the renderer for the client's text box input.

-}

module Client.Image.Textbox where

import           Client.Configuration
import           Client.Commands
import           Client.Commands.Arguments
import           Client.Image.Arguments
import           Client.Image.MircFormatting
import           Client.Image.Palette
import           Client.State
import qualified Client.State.EditBox as Edit
import           Control.Lens
import           Data.Char
import           Data.List
import qualified Data.Text as Text
import           Graphics.Vty.Image

textboxImage :: ClientState -> (Int, Image)
textboxImage st
  = (pos, croppedImage)
  where
  width = view clientWidth st
  (txt, content) =
     views (clientTextBox . Edit.content) (renderContent pal) st

  pos = computeCharWidth (width-1) txt

  pal = view (clientConfig . configPalette) st

  lineImage = beginning <|> content <|> ending

  leftOfCurWidth = myWcswidth txt

  croppedImage
    | leftOfCurWidth < width = lineImage
    | otherwise = cropLeft width (cropRight (leftOfCurWidth+1) lineImage)

  attr      = view (clientConfig . configPalette . palTextBox) st
  beginning = char attr '^'
  ending    = char attr '$'

renderContent :: Palette -> Edit.Content -> (String, Image)
renderContent pal c = (txt, wholeImg)
  where
  as  = reverse (view Edit.above c)
  bs  = view Edit.below c
  cur = view Edit.line c

  leftCur = take (view Edit.pos cur) (view Edit.text cur)

  inputLines = as ++ view Edit.text cur : bs

  -- ["one","two"] "three" --> "^two one three"
  txt = '^' : foldl (\acc x -> x++' ':acc) leftCur as

  wholeImg = horizCat
           $ intersperse (plainText "\n")
           $ map (renderLine pal) inputLines

-- | Compute the number of code-points that will be visible
-- when the given string is truncated to fit in the given
-- number of terminal columns.
computeCharWidth ::
  Int    {- ^ rendered width           -} ->
  String {- ^ input string             -} ->
  Int    {- ^ codepoints that will fit -}
computeCharWidth = go 0
  where
    go !acc _ [] = acc
    go acc 0 _ = acc
    go acc w (x:xs)
      | z > w = acc + w -- didn't fit, will be filled in
      | otherwise = go (acc+1) (w-z) xs
      where
        z = myWcwidth x

-- | Version of 'wcwidth' that accounts for how control characters are
-- rendered
myWcwidth :: Char -> Int
myWcwidth x
  | isControl x = 1
  | otherwise   = wcwidth x

-- | Version of 'wcswidth' that accounts for how control characters are
-- rendered
myWcswidth :: String -> Int
myWcswidth = sum . map myWcwidth


renderLine :: Palette -> String -> Image

renderLine pal ('/':xs)
  | (cmd,rest)            <- break isSpace xs
  , Just (Command spec _) <- view (at (Text.pack cmd)) commands
  , let attr =
          case parseArguments spec rest of
            Nothing -> view palCommand      pal
            Just{}  -> view palCommandReady pal
  = char defAttr '/' <|>
    string attr cmd <|>
    argumentsImage pal spec rest

renderLine _ xs = parseIrcTextExplicit (Text.pack xs)
