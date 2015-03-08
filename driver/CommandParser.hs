{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CommandParser
  ( Parser
  , runParser
  , pChannel
  , pNick
  , pRemaining
  , pRemainingNoSp
  , pToken
  , pValidToken
  , pTarget
  , pCommand
  , pChar
  , pSatisfy
  , pHaskell
  ) where

import Data.Char
import Control.Monad
import Control.Lens
import Control.Applicative
import Graphics.Vty.Image hiding ((<|>))
import qualified Graphics.Vty.Image as Vty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import ClientState
import Irc.Model
import Irc.Format
import HaskellHighlighter
import ImageUtils

-- tokens
--   single letter
--   free-form
--   identifiers
--     users
--     channels
--       active
--       inactive
--   mask
--
-- free-form words
--
-- render to image


newtype Parser a = Parser (String -> (String, Image, Bool, Maybe a))
  deriving (Functor)

runParser :: Parser a -> String -> (Image, Maybe a)
runParser (Parser p) s
  | all isSpace rest && commit = (img Vty.<|> string defAttr rest, res)
  | otherwise = (img Vty.<|> string (withForeColor defAttr red) rest, Nothing)
  where
  (rest,img,commit,res) = p s

instance Applicative Parser where
  pure x = Parser (\s -> (s,emptyImage,False,Just x))
  Parser f <*> Parser x = Parser (\s -> case f s of
                                          (s1,i1,c1,r1) ->
                                             case x s1 of
                                               (s2,i2,c2,r2) ->
                                                 let c3 = c1 || has _Just r1 && c2
                                                 in (s2,i1 Vty.<|> i2,c3,r1<*>r2))

instance Alternative Parser where
  empty = Parser (\s -> (s,emptyImage,False,Nothing))
  Parser x <|> Parser y = Parser $ \s ->
    case (x s,y s) of
      (rx@(_,_,True,_),_)-> rx
      ((_,_,_,_),ry@(_,_,True,_)) -> ry
      (rx,_) -> rx

pValidToken :: String -> (String -> Maybe a) -> Parser a
pValidToken name validate = Parser $ \s ->
  let (w,s1) = span (==' ') s
      (t,s2) = break (==' ') s1
      img c  = string (withForeColor defAttr c) (w ++ t)
  in if null t
       then ("", char defAttr ' ' Vty.<|>
                 string (withStyle defAttr reverseVideo) name Vty.<|>
                 string defAttr (drop (length name + 1) w)
              , False
              , Nothing)
       else case validate t of
              Just x -> (s2, img green, True, Just x)
              Nothing -> (s2, img red, True, Nothing)

pToken :: String -> Parser String
pToken name = pValidToken name Just

pRemaining :: Parser String
pRemaining = Parser (\s -> ("", string defAttr s, True, Just s))

pRemainingNoSp :: Parser String
pRemainingNoSp = fmap (dropWhile isSpace) pRemaining

pChannel :: ClientState -> Parser Identifier
pChannel st = pValidToken "channel" $ \chan ->
                do let ident = asIdentifier chan
                   guard (isChannelName ident (view clientConnection st))
                   return ident

pTarget :: Parser Identifier
pTarget = pValidToken "target" (Just . asIdentifier)

pNick :: ClientState -> Parser Identifier
pNick st = pValidToken "nick" $ \nick ->
                do let ident = asIdentifier nick
                   guard (isNickName ident (view clientConnection st))
                   return ident

asIdentifier :: String -> Identifier
asIdentifier = mkId . Text.encodeUtf8 . Text.pack

pChar :: Char -> Parser Char
pChar c = pSatisfy [c] (== c)

pSatisfy :: String -> (Char -> Bool) -> Parser Char
pSatisfy name f = Parser (\s ->
          case s of
            c1:s1 | f c1 -> (s1, char defAttr c1, True, Just c1)
                  | otherwise -> (s1, emptyImage, False, Nothing)
            [] -> (s, string (withStyle defAttr reverseVideo) (' ':name), False, Nothing))

pCommand :: String -> Parser ()
pCommand cmd = Parser (\s ->
          let (t,s1) = break (==' ') s
          in if cmd == t
                then (s1, string (withForeColor defAttr yellow) t, True, Just ())
                else (s, emptyImage, False, Nothing))

pHaskell :: Parser String
pHaskell = Parser (\s ->
            ("", cleanText (Text.pack (highlightHaskell s)), True,
                        Just (drop 1 (highlightHaskell s))))
