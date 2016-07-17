{-# Language RankNTypes #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
module Config.FromConfig
  ( -- * Configuration parsing
    ConfigParser
  , decodeConfig
  , runConfigParser
  , failure
  , FromConfig(parseConfig)

  -- * Section parsing
  , SectionParser
  , parseSections
  , sectionReq
  , sectionOpt
  , sectionOptWith
  , liftConfigParser
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
import           Data.Text (Text)
import qualified Data.Text as Text

newtype ConfigParser a = ConfigParser (ReaderT [Text] (Either Text) a)
  deriving (Functor, Applicative, Monad)

runConfigParser :: ConfigParser a -> Either Text a
runConfigParser (ConfigParser p) = runReaderT p []

failure :: Text -> ConfigParser a
failure msg = ConfigParser $
  do loc <- ask
     let msg' = Text.concat [Text.intercalate "." (reverse loc), ": ", msg]
     lift (Left msg')

extendLoc :: Text -> ConfigParser a -> ConfigParser a
extendLoc loc (ConfigParser p) = ConfigParser (local (loc:) p)

------------------------------------------------------------------------

decodeConfig :: FromConfig a => Value -> Either Text a
decodeConfig = runConfigParser . parseConfig

class FromConfig a where
  parseConfig :: Value -> ConfigParser a

instance FromConfig Text where
  parseConfig (Text x)          = return x
  parseConfig _                 = failure "expected text"

instance FromConfig Integer where
  parseConfig (Number _ n)      = return n
  parseConfig _                 = failure "expected number"

instance FromConfig Atom where
  parseConfig (Atom a)          = return a
  parseConfig _                 = failure "expected atom"

instance FromConfig a => FromConfig [a] where
  parseConfig (List xs)         = ifor xs $ \i x ->
                                    extendLoc (Text.pack (show (i+1)))
                                              (parseConfig x)
  parseConfig _                 = failure "expected list"

------------------------------------------------------------------------

newtype SectionParser a =
  SectionParser (StateT (HashMap Text Value) ConfigParser a)
  deriving (Functor, Applicative, Monad)

liftConfigParser :: ConfigParser a -> SectionParser a
liftConfigParser = SectionParser . lift

parseSections :: SectionParser a -> Value -> ConfigParser a
parseSections (SectionParser p) (Sections xs) =
  do (res, xs') <- runStateT p (toHashMap xs)
     let unused = HashMap.keys xs'
     unless (null unused)
       (failure ("unknown keys: " <> Text.intercalate ", " unused))
     return res
parseSections _ _ = failure "expected sections"

sectionOpt :: FromConfig a => Text -> SectionParser (Maybe a)
sectionOpt = sectionOptWith parseConfig

-- Parses the value stored at the given section with the given parser.
-- Nothing is returned if the section is missing.
-- Just is returned if the parse succeeds
-- Error is raised if the section is present but the parse fails
sectionOptWith :: (Value -> ConfigParser a) -> Text -> SectionParser (Maybe a)
sectionOptWith p key = SectionParser $
  do mb <- at key <<.= Nothing
     lift (traverse (extendLoc key . p) mb)

sectionReq :: FromConfig a => Text -> SectionParser a
sectionReq key =
  do mb <- sectionOpt key
     liftConfigParser $ case mb of
                          Nothing -> failure ("section required: " <> key)
                          Just x  -> return x

toHashMap :: [Section] -> HashMap Text Value
toHashMap xs = HashMap.fromList [ (k,v) | Section k v <- xs ] -- todo: handle duplicate sections
