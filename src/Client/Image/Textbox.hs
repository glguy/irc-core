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
import           Client.Image.Message
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Focus
import qualified Client.State.EditBox as Edit
import           Control.Lens
import           Data.Char
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import           Data.List
import           Data.Semigroup
import qualified Data.Text as Text
import           Graphics.Vty.Attributes
import qualified Graphics.Vty.Image as Vty
import           Irc.Identifier

-- | Compute the UI image for the text input box. This computes
-- the logical cursor position on the screen to compensate for
-- VTY's cursor placement behavior.
textboxImage :: Int -> ClientState -> (Int, Int, Vty.Image) -- ^ cursor column, new offset, image
textboxImage width st
  = (newPos, newOffset, croppedImage)
  where
  macros = views (clientConfig . configMacros) (fmap macroSpec) st
  (txt, content) =
     views (clientTextBox . Edit.content) (renderContent myNick nicks macros pal) st

  lineImage = unpackImage (beginning <> content <> ending)

  leftOfCurWidth = myWcswidth ('^':txt)

  croppedImage = Vty.resizeWidth width
               $ Vty.cropLeft (Vty.imageWidth lineImage - newOffset) lineImage

  cursorAnchor = width * 3 `quot` 4

  -- previous offset value
  oldOffset = view clientTextBoxOffset st

  -- position based on old offset
  oldPos = leftOfCurWidth - oldOffset

  -- new offset (number of columns to trim from left side of text box)
  newOffset
    | 0 <= oldPos, oldPos < width = oldOffset
    | otherwise                   = max 0 (leftOfCurWidth - cursorAnchor)

  newPos = leftOfCurWidth - newOffset

  pal       = clientPalette st
  attr      = view palTextBox pal
  beginning = char attr '^'
  ending    = char attr '$'


  (myNick,nicks) =
    case view clientFocus st of
      ChannelFocus network channel ->
         (clientHighlightsNetwork network st,
          HashSet.fromList (channelUserList network channel st)
         )
      _ -> (HashSet.empty, HashSet.empty)


-- | Renders the whole, uncropped text box as well as the 'String'
-- corresponding to the rendered image which can be used for computing
-- the logical cursor position of the cropped version of the text box.
renderContent ::
  HashSet Identifier ->
  HashSet Identifier ->
  Recognizer MacroSpec {- ^ macro completions                     -} ->
  Palette              {- ^ palette                               -} ->
  Edit.Content         {- ^ content                               -} ->
  (String, Image')     {- ^ plain text rendering, image rendering -}
renderContent myNick nicks macros pal c = (txt, wholeImg)
  where
  as  = reverse (view Edit.above c)
  bs  = view Edit.below c
  cur = view Edit.line c

  curTxt  = view Edit.text cur
  leftCur = take (view Edit.pos cur) (view Edit.text cur)

  -- ["one","two"] "three" --> "two one three"
  txt = foldl (\acc x -> x ++ ' ' : acc) leftCur as

  render = renderLine pal myNick nicks macros

  wholeImg = mconcat
           $ intersperse (plainText "\n")
           $ map (render False) as
          ++ render True curTxt
           : map (render False) bs


-- | Version of 'wcwidth' that accounts for how control characters are
-- rendered
myWcwidth :: Char -> Int
myWcwidth x
  | isControl x = 1
  | otherwise   = Vty.wcwidth x

-- | Version of 'wcswidth' that accounts for how control characters are
-- rendered
myWcswidth :: String -> Int
myWcswidth = sum . map myWcwidth


-- | Render the active text box line using command highlighting and
-- placeholders, and WYSIWYG mIRC formatting control characters.
renderLine ::
  Palette ->
  HashSet Identifier ->
  HashSet Identifier ->
  Recognizer MacroSpec {- ^ commands     -} ->
  Bool                 {- ^ focused      -} ->
  String               {- ^ input text   -} ->
  Image'               {- ^ output image -}
renderLine pal myNick nicks macros focused ('/':xs) =
  char defAttr '/' <> string attr cmd <> continue rest
  where
    specAttr spec =
      case parseArguments spec rest of
        Nothing -> view palCommand      pal
        Just{}  -> view palCommandReady pal

    (cmd, rest) = break isSpace xs
    allCommands = (Left <$> macros) <> (Right <$> commands)
    (attr, continue)
      = case recognize (Text.pack cmd) allCommands of
          Exact (Right Command{cmdArgumentSpec = spec}) ->
            ( specAttr spec
            , argumentsImage pal spec focused
            )
          Exact (Left (MacroSpec spec)) ->
            ( specAttr spec
            , argumentsImage pal spec focused
            )
          Prefix _ ->
            ( view palCommandPrefix pal
            , parseIrcTextWithNicks pal myNick nicks focused . Text.pack
            )
          Invalid ->
            ( view palCommandError pal
            , parseIrcTextWithNicks pal myNick nicks focused . Text.pack
            )

renderLine pal myNick nicks _ focused xs = parseIrcTextWithNicks pal myNick nicks focused (Text.pack xs)
