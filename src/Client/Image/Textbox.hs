{-# Language BangPatterns #-}

{-|
Module      : Client.Image.Textbox
Description : Textbox renderer
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides the renderer for the client's text box input.

-}

module Client.Image.Textbox
  ( textboxImage
  ) where

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

-- | Compute the UI image for the text input box. This computes
-- the logical cursor position on the screen to compensate for
-- VTY's cursor placement behavior.
textboxImage :: ClientState -> (Int, Image) -- ^ cursor column, image
textboxImage st
  = (pos, croppedImage)
  where
  width = view clientWidth st
  (txt, content) =
     views (clientTextBox . Edit.content) (renderContent pal) st

  pos = min (width-1) leftOfCurWidth

  pal = view (clientConfig . configPalette) st

  lineImage = beginning <|> content <|> ending

  leftOfCurWidth = myWcswidth ('^':txt)

  croppedImage
    | leftOfCurWidth < width = lineImage
    | otherwise = cropLeft width (cropRight (leftOfCurWidth+1) lineImage)

  attr      = view (clientConfig . configPalette . palTextBox) st
  beginning = char attr '^'
  ending    = char attr '$'

-- | Renders the whole, uncropped text box as well as the 'String'
-- corresponding to the rendered image which can be used for computing
-- the logical cursor position of the cropped version of the text box.
renderContent ::
  Palette         {- ^ palette                               -} ->
  Edit.Content    {- ^ content                               -} ->
  (String, Image) {- ^ plain text rendering, image rendering -}
renderContent pal c = (txt, wholeImg)
  where
  as  = reverse (view Edit.above c)
  bs  = view Edit.below c
  cur = view Edit.line c

  curTxt  = view Edit.text cur
  leftCur = take (view Edit.pos cur) (view Edit.text cur)

  -- ["one","two"] "three" --> "two one three"
  txt = foldl (\acc x -> x ++ ' ' : acc) leftCur as

  wholeImg = horizCat
           $ intersperse (plainText "\n")
           $ map renderOtherLine as
          ++ renderLine pal curTxt
           : map renderOtherLine bs


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


-- | Render an unfocused line
renderOtherLine :: String -> Image
renderOtherLine = parseIrcTextExplicit . Text.pack

-- | Render the active text box line using command highlighting and
-- placeholders, and WYSIWYG mIRC formatting control characters.
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
