{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}

{-|
Module      : Config.FromConfig
Description : Parser for unstructure configuration file format
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides tools for producing structured configuration
information out of configuration values.

-}
module Config.FromConfig
  ( -- * Configuration parsing
    ConfigParser
  , decodeConfig
  , runConfigParser
  , failure
  , extendLoc
  , FromConfig(parseConfig)

  -- * Parser wrappers
  , parseList

  -- * Section parsing
  , SectionParser
  , parseSections
  , sectionReq
  , sectionReqWith
  , sectionOpt
  , sectionOptWith
  , liftConfigParser
  , parseSectionsWith
  ) where

import           Config
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Configuration parser tracking current location and for propagating
-- error information.
newtype ConfigParser a = ConfigParser (ReaderT [Text] (Either Text) a)
  deriving (Functor, Applicative, Monad)

-- | Run a top-level parser to either get the parsed value or an error message.
runConfigParser :: ConfigParser a -> Either Text a
runConfigParser (ConfigParser p) = runReaderT p []

-- | A parser that always fails with the given error message.
failure :: Text {- ^ error message -} -> ConfigParser a
failure msg = ConfigParser $
  do loc <- ask
     let msg' = Text.concat [Text.intercalate "." (reverse loc), ": ", msg]
     lift (Left msg')

-- | Embed a parser into an extended location. This is used when
-- parsing inside a section.
extendLoc :: Text -> ConfigParser a -> ConfigParser a
extendLoc loc (ConfigParser p) = ConfigParser (local (loc:) p)

------------------------------------------------------------------------

-- | Parse a 'Value' according to the method in the 'FromConfig'
decodeConfig :: FromConfig a => Value -> Either Text a
decodeConfig = runConfigParser . parseConfig

-- | Class for types that have a well-known way to parse them.
class FromConfig a where
  -- | Parse a value
  parseConfig :: Value -> ConfigParser a

-- | Matches 'Text' values.
instance FromConfig Text where
  parseConfig (Text x)          = return x
  parseConfig _                 = failure "expected text"

-- | Matches @yes@ as 'True' and @no@ as 'False.
instance FromConfig Bool where
  parseConfig (Atom "yes")      = return True
  parseConfig (Atom "no")       = return False
  parseConfig _                 = failure "expected yes or no"

-- | Matches 'Number' values ignoring the base
instance FromConfig Integer where
  parseConfig (Number _ n)      = return n
  parseConfig (Floating c e)
    | denominator n == 1 = return $! numerator n
    where
      n = floatingToRatio c e
  parseConfig _                 = failure "expected integral number"

instance FromConfig Int where
  parseConfig v =
    do i <- parseConfig v
       let small = minBound :: Int
           large = maxBound :: Int
       when (i < toInteger small || toInteger large < i)
          (failure "int out of range")
       return (fromInteger i)

-- | Matches 'Number' values ignoring the base
instance Integral a => FromConfig (Ratio a) where
  parseConfig (Number _ n)   = return $! fromIntegral n
  parseConfig (Floating c e) = return $! floatingToRatio c e
  parseConfig _              = failure "expected fractional number"

-- | Matches 'Atom' values
instance FromConfig Atom where
  parseConfig (Atom a)          = return a
  parseConfig _                 = failure "expected atom"

-- | Matches 'List' values, extends the error location with a zero-based
-- index
instance FromConfig a => FromConfig [a] where
  parseConfig = parseList parseConfig

------------------------------------------------------------------------

-- | Parser for consuming key-value pairs of sections.
newtype SectionParser a =
  SectionParser (StateT (HashMap Text Value) ConfigParser a)
  deriving (Functor, Applicative, Monad)

-- | Lift a 'ConfigParser' into a 'SectionParser' leaving the current
-- section information unmodified.
liftConfigParser :: ConfigParser a -> SectionParser a
liftConfigParser = SectionParser . lift

-- | Run a 'SectionParser' given particular 'Value'. This will only
-- succeed when the value is a 'Sections' and the section parser consumes all
-- of the sections from that value.
parseSections :: SectionParser a -> Value -> ConfigParser a
parseSections (SectionParser p) (Sections xs) =
  do (res, xs') <- runStateT p (toHashMap xs)
     let unused = HashMap.keys xs'
     unless (null unused)
       (failure ("unknown keys: " <> Text.intercalate ", " unused))
     return res
parseSections _ _ = failure "expected sections"

-- |
-- @
-- sectionOpt = sectionOptWith parseConfig
-- @
sectionOpt :: FromConfig a => Text -> SectionParser (Maybe a)
sectionOpt = sectionOptWith parseConfig

-- | Parses the value stored at the given section with the given parser.
-- Nothing is returned if the section is missing.
-- Just is returned if the parse succeeds
-- Error is raised if the section is present but the parse fails
sectionOptWith :: (Value -> ConfigParser a) -> Text -> SectionParser (Maybe a)
sectionOptWith p key = SectionParser $
  do mb <- at key <<.= Nothing
     lift (traverse (extendLoc key . p) mb)

-- | Parse the value at the given section or fail.
sectionReq :: FromConfig a => Text -> SectionParser a
sectionReq = sectionReqWith parseConfig

-- | Parse the value at the given section or fail.
sectionReqWith :: (Value -> ConfigParser a) -> Text -> SectionParser a
sectionReqWith p key =
  do mb <- sectionOptWith p key
     liftConfigParser $ case mb of
                          Nothing -> failure ("section required: " <> key)
                          Just x  -> return x

toHashMap :: [Section] -> HashMap Text Value
toHashMap xs = HashMap.fromList [ (k,v) | Section k v <- xs ] -- todo: handle duplicate sections

floatingToRatio :: Integral a => Integer -> Integer -> Ratio a
floatingToRatio c e = fromIntegral c * 10 ^^ e

parseSectionsWith :: (a -> Text -> Value -> ConfigParser a) -> a -> Value -> ConfigParser a
parseSectionsWith p start s =
  case s of
    Sections xs -> foldM (\x (Section k v) -> extendLoc k (p x k v)) start xs
    _ -> failure "Expected sections"

parseList :: (Value -> ConfigParser a) -> Value -> ConfigParser [a]
parseList p (List xs) = ifor xs $ \i x ->
                          extendLoc (Text.pack (show (i+1))) (p x)
parseList _ _         = failure "expected list"
