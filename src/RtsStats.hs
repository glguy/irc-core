{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, DeriveGeneric, TypeOperators, StandaloneDeriving, CPP, EmptyCase #-}
{-# OPTIONS_GHC -Wno-orphans -funfolding-use-threshold=2000 -funfolding-creation-threshold=1000 #-}

{-|
Module      : RtsStats
Description : Compatibility layer for GHC RTS statistics across versions
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}

module RtsStats
  ( Stats
  , getStats
  , statsToEntries
  ) where

import           Data.Int
import           Data.List (intercalate)
import           Data.List.Split (chunksOf)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word
import           GHC.Generics
import           GHC.Stats

type KVs = [(Text,Text)] -> [(Text,Text)]

cons :: (Text, Text) -> KVs
cons = (:)

class    Fields a      where fields :: a -> Text -> KVs
instance Fields Int64  where fields = commaFields
instance Fields Word32 where fields = commaFields
instance Fields Word64 where fields = commaFields
instance Fields Double where fields = showFields

showFields :: Show a => a -> Text -> KVs
showFields x n = cons (n, Text.pack (show x))
{-# NOINLINE showFields #-}

commaFields :: Show a => a -> Text -> KVs
commaFields x n = cons (n, Text.pack (addCommas (show x)))
{-# NOINLINE commaFields #-}

addCommas :: String -> String
addCommas = reverse . intercalate "," . chunksOf 3 . reverse

genericFields :: (Generic a, GFields (Rep a)) => a -> Text -> KVs
genericFields = gfields . from

class GFields f where
  gfields :: f p -> Text -> KVs

instance GFields f => GFields (D1 c f) where
  gfields (M1 x) = gfields x
  {-# INLINE gfields #-}

instance GFields f => GFields (C1 c f) where
  gfields (M1 x) = gfields x
  {-# INLINE gfields #-}

instance (Selector s, GFields f) => GFields (S1 s f) where
  gfields s@(M1 x) _ = gfields x (Text.pack (selName s))
  {-# INLINE gfields #-}

instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields (x :*: y) n = gfields x n . gfields y n
  {-# INLINE gfields #-}

instance (GFields f, GFields g) => GFields (f :+: g) where
  gfields (L1 x) = gfields x
  gfields (R1 x) = gfields x
  {-# INLINE gfields #-}

instance GFields U1 where
  gfields _ _ = id
  {-# INLINE gfields #-}

instance GFields V1 where
  gfields v _ = case v of {}
  {-# INLINE gfields #-}

instance Fields a => GFields (K1 i a) where
  gfields (K1 x) = fields x
  {-# INLINE gfields #-}

statsToEntries :: Stats -> [(Text, Text)]
statsToEntries (Stats rts) = fields rts "stats" []

#if MIN_VERSION_base(4,10,0)

newtype Stats = Stats RTSStats

deriving instance Generic RTSStats
deriving instance Generic GCDetails
instance Fields RTSStats  where fields = genericFields
instance Fields GCDetails where fields = genericFields

getStats :: IO (Maybe Stats)
getStats =
  do enabled <- getRTSStatsEnabled
     if enabled then Just . Stats <$> getRTSStats
                else pure Nothing

#else

newtype Stats = Stats GCStats

deriving instance Generic GCStats
instance Fields GCStats where fields = genericFields

getStats :: IO (Maybe Stats)
getStats =
  do enabled <- getGCStatsEnabled
     if enabled then Just . Stats <$> getGCStats
                else pure Nothing

#endif
