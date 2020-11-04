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
import           Client.Commands.Arguments.Renderer
import           Client.Commands.Arguments.Parser
import           Client.Commands.Interpolation
import           Client.Commands.Recognizer
import           Client.Image.Message
import           Client.Image.MircFormatting
import           Client.Image.PackedImage
import           Client.Image.Palette
import           Client.State
import           Client.State.Focus
import qualified Client.State.EditBox as Edit
import           Control.Lens
import qualified Data.HashSet as HashSet
import           Data.HashSet (HashSet)
import           Data.List
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
     renderContent st myNick nicks macros pal
       (view (clientTextBox . Edit.content) st)

  lineImage = unpackImage (beginning <> content <> ending)

  leftOfCurWidth = 1 + txt

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
  ClientState          {- ^ client state                          -} ->
  HashSet Identifier   {- ^ my nicknames                          -} ->
  HashSet Identifier   {- ^ other nicknames                       -} ->
  Recognizer MacroSpec {- ^ macro completions                     -} ->
  Palette              {- ^ palette                               -} ->
  Edit.Content         {- ^ content                               -} ->
  (Int, Image')        {- ^ left-of-cursor width, image rendering -}
renderContent st myNick nicks macros pal c = (leftLen, wholeImg)
  where
  as  = reverse (view Edit.above c)
  bs  = view Edit.below c
  cur = view Edit.line c

  curTxt  = view Edit.text cur
  leftCur = take (view Edit.pos cur) (view Edit.text cur)

  -- ["one","two"] "three" --> "two one three"
  leftLen = length leftImgs -- separators
          + sum (map imageWidth leftImgs)
          + imageWidth (parseIrcText' True (Text.pack leftCur))

  rndr = renderLine st pal myNick nicks macros

  leftImgs = map (rndr False) as

  wholeImg = mconcat
           $ intersperse (plainText "\n")
           $ leftImgs
          ++ rndr True curTxt
           : map (rndr False) bs


-- | Render the active text box line using command highlighting and
-- placeholders, and WYSIWYG mIRC formatting control characters.
renderLine ::
  ClientState ->
  Palette ->
  HashSet Identifier ->
  HashSet Identifier ->
  Recognizer MacroSpec {- ^ commands     -} ->
  Bool                 {- ^ focused      -} ->
  String               {- ^ input text   -} ->
  Image'               {- ^ output image -}
renderLine st pal myNick nicks macros focused input =

  case span (' '==) input of
    (spcs, '/':xs) -> string defAttr spcs <> char defAttr '/'
                   <> string attr cleanCmd <> continue rest
      where
        specAttr spec =
          case parse st spec rest of
            Nothing -> view palCommand      pal
            Just{}  -> view palCommandReady pal

        (cmd, rest) = break (' '==) xs
        cleanCmd = map cleanChar cmd

        allCommands = (Left <$> macros) <> (Right <$> commands)
        (attr, continue)
          = case recognize (Text.toLower (Text.pack cmd)) allCommands of
              Exact (Right Command{cmdArgumentSpec = spec}) ->
                ( specAttr spec
                , render pal st focused spec
                )
              Exact (Left (MacroSpec spec)) ->
                ( specAttr spec
                , render pal st focused spec
                )
              Prefix _ ->
                ( view palCommandPrefix pal
                , parseIrcTextWithNicks pal myNick nicks focused . Text.pack
                )
              Invalid ->
                ( view palCommandError pal
                , parseIrcTextWithNicks pal myNick nicks focused . Text.pack
                )
    _ -> parseIrcTextWithNicks pal myNick nicks focused (Text.pack input)
