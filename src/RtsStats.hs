{-# Language OverloadedStrings #-}

{-|
Module      : RtsStats
Description : Human readable interface to GHC RTS statistics
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}

module RtsStats
  ( Stats
  , getStats
  , statsToEntries
  ) where

import Data.Int        (Int64)
import Data.List       (intercalate)
import Data.List.Split (chunksOf)
import Data.Text       (Text)
import Data.Text qualified as Text
import Data.Word       (Word32, Word64)
import GHC.Stats

class    Render a      where render :: a -> Text
instance Render Word32 where render = prettyNum
instance Render Word64 where render = prettyNum
instance Render Int64  where render = prettyNum
instance Render Double where render = Text.pack . show

prettyNum :: Show a => Num a => Ord a => a -> Text
prettyNum n = Text.pack (addSign (addCommas (show (abs n))))
  where
    addSign = if n < 0 then (:) '-' else id

addCommas :: String -> String
addCommas = reverse . intercalate "," . chunksOf 3 . reverse

newtype Stats = Stats RTSStats

getStats :: IO (Maybe Stats)
getStats =
  do enabled <- getRTSStatsEnabled
     if enabled then Just . Stats <$> getRTSStats
                else pure Nothing

statsToEntries :: Stats -> [(Text, Text)]
statsToEntries (Stats rts) =
  let rgc = gc rts in seq rgc
  [ ("GCs"                                 , render $ gcs                             rts)
  , ("Major GCs"                           , render $ major_gcs                       rts)
  , ("Allocated bytes"                     , render $ allocated_bytes                 rts)
  , ("Max live bytes"                      , render $ max_live_bytes                  rts)
  , ("Max large objects bytes"             , render $ max_large_objects_bytes         rts)
  , ("Max compact bytes"                   , render $ max_compact_bytes               rts)
  , ("Max slop bytes"                      , render $ max_slop_bytes                  rts)
  , ("Max memory in use bytes"             , render $ max_mem_in_use_bytes            rts)
  , ("Cumulative live bytes"               , render $ cumulative_live_bytes           rts)
  , ("Copied bytes"                        , render $ copied_bytes                    rts)
  , ("Parallel copied bytes"               , render $ par_copied_bytes                rts)
  , ("Cumulative parallel max copied bytes", render $ cumulative_par_max_copied_bytes rts)
  , ("Mutator CPU ns"                      , render $ mutator_cpu_ns                  rts)
  , ("Mutator elapsed ns"                  , render $ mutator_elapsed_ns              rts)
  , ("GC CPU ns"                           , render $ gc_cpu_ns                       rts)
  , ("GC elapsed ns"                       , render $ gc_elapsed_ns                   rts)
  , ("CPU ns"                              , render $ cpu_ns                          rts)
  , ("Elapsed ns"                          , render $ elapsed_ns                      rts)
  , ("==[Totals]=="                        , ""                                          )
  , (""                                    , ""                                          )
  , ("Generation"                          , render $ gcdetails_gen                   rgc)
  , ("Threads"                             , render $ gcdetails_threads               rgc)
  , ("Allocated bytes"                     , render $ gcdetails_allocated_bytes       rgc)
  , ("Live bytes"                          , render $ gcdetails_live_bytes            rgc)
  , ("Large objects bytes"                 , render $ gcdetails_large_objects_bytes   rgc)
  , ("Compact bytes"                       , render $ gcdetails_compact_bytes         rgc)
  , ("Slop bytes"                          , render $ gcdetails_slop_bytes            rgc)
  , ("Memory in use bytes"                 , render $ gcdetails_mem_in_use_bytes      rgc)
  , ("Copied bytes"                        , render $ gcdetails_copied_bytes          rgc)
  , ("Parallel max copied bytes"           , render $ gcdetails_par_max_copied_bytes  rgc)
  , ("Sync elapsed ns"                     , render $ gcdetails_sync_elapsed_ns       rgc)
  , ("CPU ns"                              , render $ gcdetails_cpu_ns                rgc)
  , ("Elapsed ns"                          , render $ gcdetails_elapsed_ns            rgc)
  , ("==[Last GC]=="                       , ""                                          )
  ]
