{-# Language CPP, OverloadedStrings #-}

{-|
Module      : Client.Docs
Description : Compile-time documentation injection
Copyright   : (c) TheDaemoness 2023
License     : ISC
Maintainer  : emertens@gmail.com

This module adds the requisite functions to load and parse
a subset of AsciiDoc and embed it using Template Haskell.
-}
module Client.Docs
  ( Docs
  , loadDoc
  , lookupDoc
  , makeHeader
  ) where

import           Prelude hiding (readFile)

import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as Parse
import           Data.ByteString (readFile)
import           Data.Char (isSpace)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Text.Encoding (decodeUtf8)
import           Language.Haskell.TH (Exp, Q, runIO)
import           Language.Haskell.TH.Syntax (lift)
import           System.FilePath ((</>), (<.>))

#if MIN_VERSION_template_haskell(2,19,0)
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)
addRelativeDependentFile :: FilePath -> Q ()
addRelativeDependentFile relPath = makeRelativeToProject relPath >>= addDependentFile
#else
addRelativeDependentFile :: FilePath -> Q ()
addRelativeDependentFile _ = return ()
#endif

type Docs = HashMap String LText.Text

data Line
  = Discarded
  | Section Text
  | Subsection Text
  | Contents LText.Text

makeHeader :: LText.Text -> LText.Text
makeHeader header = LText.append "\^B" (LText.append header ":\^B\n")

loadDoc :: (String -> String) -> FilePath -> Q Docs
loadDoc keymod path = addRelativeDependentFile splicePath >> runIO (readFile splicePath >>= renderDoc)
  where
    splicePath = "doc" </> path <.> "adoc"
    renderDoc doc = case Parse.parseOnly lineParser $ decodeUtf8 doc of
      Right docs -> return $ buildDocs keymod docs
      Left errorMsg -> fail ("Parser failed on `" ++ splicePath ++ "`: " ++ errorMsg)

lookupDoc :: LText.Text -> String -> Docs -> Q Exp
lookupDoc header name docs =
  case HashMap.lookup name docs of
    Just doc -> lift $ LText.toStrict $ LText.append header doc
    Nothing -> fail failMsg
  where
    failMsg = "No docs for `" ++ name ++ "` (have " ++ show (HashMap.keys docs) ++ ")"

buildDocs :: (String -> String) -> [Line] -> Docs
buildDocs keymod parsedLines = docs
  where
    folded = foldl (addLine keymod) (HashMap.empty, "", LText.empty) parsedLines
    (docs, _, _) = addLine keymod folded (Section "")

data RenderContentsState
  = NormalState
  | CodeStartState
  | CodeEndState
  | CodeBlockState

renderContents :: RenderContentsState -> LText.Text -> LText.Text
renderContents state = Builder.toLazyText . snd . foldl renderContents' (state, mempty) . LText.unpack
  where
    renderContents' (st, text) char = case (st, char) of
       (CodeStartState, '+') -> (CodeBlockState, text <> Builder.fromText "\^_")
       (CodeStartState, _  ) -> (NormalState,    text <> Builder.fromText "\^B" <> Builder.singleton char)
       (CodeEndState,   '`') -> (NormalState,    text <> Builder.fromText "\^_")
       (CodeEndState,   _  ) -> (CodeBlockState, text <> Builder.singleton '+' <> Builder.singleton char)
       (NormalState,    '`') -> (CodeStartState, text)
       (CodeBlockState, '+') -> (CodeEndState,   text)
       (_,              _  ) -> (st,             text <> Builder.singleton char)

addLine :: (String -> String) -> (Docs, Text, LText.Text) -> Line -> (Docs, Text, LText.Text)
addLine _      (docs, section, text)      Discarded    = (docs, section, text)
addLine _      (docs, "", _)              (Section s') = (docs, s', LText.empty)
addLine _      (docs, "", text)           _            = (docs, "", text)
addLine keymod (docs, section, text) line              = case line of
  -- TODO: Keep renderContents state across lines.
  -- Otherwise start in NormalState after each newline.
  Contents text'   -> (docs, section, append' $ renderContents NormalState text')
  Subsection text' -> (docs, section, append' (makeHeader (LText.fromStrict text')))
  Section s'       -> (HashMap.insert (keymod $ Text.unpack section) text docs, s', LText.empty)
  where
    append' = LText.append text

lineParser :: Parse.Parser [Line]
lineParser = Parse.many1' (sectionParser <|> contentsParser) <* Parse.endOfInput
  where
    sectionParser = Parse.char '=' >> (sectionL2Parser <|> sectionL3Parser <|> return Discarded)
      where
        sectionL2Parser = do
          _ <- Parse.string "= "
          name <- Parse.takeWhile1 (not . isSpace)
          eolParser
          return (Section name)
        sectionL3Parser = do
          _ <- Parse.takeWhile1 (== '=')
          Parse.skipWhile (== ' ')
          chars <- Parse.manyTill Parse.anyChar eolParser
          return (Subsection $ Text.pack chars)
    contentsParser = do
      chars <- Parse.manyTill Parse.anyChar eolParser
      return $ Contents $ LText.fromChunks ["  ", Text.pack chars, "\n"]
    eolParser = do
      spaces <- Parse.takeWhile (== ' ')
      _ <- if Text.null spaces then pure '+' else Parse.option '+' (Parse.char '+')
      Parse.endOfLine
