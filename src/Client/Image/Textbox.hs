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
import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Image.Arguments
import           Client.Image.MircFormatting
import           Client.Image.Palette
import           Client.State
import qualified Client.State.EditBox as Edit
import           Control.Lens
import           Data.Char
import           Data.List
import           Data.Monoid
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
  macros = views (clientConfig . configMacros) (fmap macroSpec) st
  (txt, content) =
     views (clientTextBox . Edit.content) (renderContent macros pal) st

  pos = min (width-1) leftOfCurWidth

  lineImage = beginning <|> content <|> ending

  leftOfCurWidth = myWcswidth ('^':txt)

  croppedImage
    | leftOfCurWidth < width = lineImage
    | otherwise = cropLeft width (cropRight (leftOfCurWidth+1) lineImage)

  pal       = clientPalette st
  attr      = view palTextBox pal
  beginning = char attr '^'
  ending    = char attr '$'

-- | Renders the whole, uncropped text box as well as the 'String'
-- corresponding to the rendered image which can be used for computing
-- the logical cursor position of the cropped version of the text box.
renderContent ::
  Recognizer MacroSpec {- ^ macro completions                     -} ->
  Palette         {- ^ palette                               -} ->
  Edit.Content    {- ^ content                               -} ->
  (String, Image) {- ^ plain text rendering, image rendering -}
renderContent macros pal c = (txt, wholeImg)
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
          ++ renderLine macros pal curTxt
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
renderLine :: Recognizer MacroSpec -> Palette -> String -> Image
renderLine macros pal ('/':xs)
  = char defAttr '/' <|> string attr cmd <|> continue rest
 where
 specAttr spec =
   case parseArguments spec rest of
     Nothing -> view palCommand      pal
     Just{}  -> view palCommandReady pal

 (cmd, rest) = break isSpace xs
 allCommands = (Left <$> macros) <> (Right <$> commands)
 (attr, continue)
   = case recognize (Text.pack cmd) allCommands of
       Exact (Right (Command _ spec _ _)) ->
         ( specAttr spec
         , argumentsImage pal spec
         )
       Exact (Left (MacroSpec spec)) ->
         ( specAttr spec
         , argumentsImage pal spec
         )
       Prefix _ ->
         ( view palCommandPrefix pal
         , renderOtherLine
         )
       Invalid ->
         ( view palCommandError pal
         , renderOtherLine
         )

renderLine _ _ xs = parseIrcTextExplicit (Text.pack xs)

