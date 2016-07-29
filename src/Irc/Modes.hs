{-# Language TemplateHaskell #-}
{-# Language BangPatterns #-}

{-|
Module      : Irc.Modes
Description : Operations for interpreting mode changes
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides support for interpreting the modes changed by
a MODE command.

-}
module Irc.Modes
  (
  -- * Interpretation of modes
    ModeTypes(..)
  , modesLists
  , modesAlwaysArg
  , modesSetArg
  , modesNeverArg
  , modesPrefixModes
  , defaultModeTypes
  , defaultUmodeTypes

  -- * Operations for working with MODE command parameters
  , splitModes
  , unsplitModes
  ) where

import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Settings that describe how to interpret channel modes
data ModeTypes = ModeTypes
  { _modesLists       :: ![Char] -- ^ modes for channel lists (e.g. ban)
  , _modesAlwaysArg   :: ![Char] -- ^ modes that always have an argument
  , _modesSetArg      :: ![Char] -- ^ modes that have an argument when set
  , _modesNeverArg    :: ![Char] -- ^ modes that never have arguments
  , _modesPrefixModes :: ![(Char,Char)] -- ^ modes requiring a nickname argument (mode,sigil)
  }
  deriving Show

makeLenses ''ModeTypes

-- | The channel modes used by Freenode
defaultModeTypes :: ModeTypes
defaultModeTypes = ModeTypes
  { _modesLists     = "eIbq"
  , _modesAlwaysArg = "k"
  , _modesSetArg    = "flj"
  , _modesNeverArg  = "CFLMPQScgimnprstz"
  , _modesPrefixModes = [('o','@'),('v','+')]
  }

-- | The default UMODE used by Freenode
defaultUmodeTypes :: ModeTypes
defaultUmodeTypes = ModeTypes
  { _modesLists     = ""
  , _modesAlwaysArg = ""
  , _modesSetArg    = "s"
  , _modesNeverArg  = "DQRZgiow"
  , _modesPrefixModes = []
  }

-- | Split up a mode change command and arguments into individual changes
-- given a configuration.
splitModes ::
  ModeTypes {- ^ mode interpretation -} ->
  Text      {- ^ modes               -} ->
  [Text]    {- ^ arguments           -} ->
  Maybe [(Bool,Char,Text)] {- ^ (set, mode, parameter) -}
splitModes !icm = computeMode True . Text.unpack
  where
  computeMode ::
    Bool   {- current polarity -} ->
    [Char] {- remaining modes -} ->
    [Text] {- remaining arguments -} ->
    Maybe [(Bool,Char,Text)]
  computeMode polarity modes args =

    case modes of
      [] | null args -> Just []
         | otherwise -> Nothing

      '+':ms -> computeMode True  ms args
      '-':ms -> computeMode False ms args

      m:ms
        |             m `elem` view modesAlwaysArg icm
       || polarity && m `elem` view modesSetArg icm
       ||             m `elem` map fst (view modesPrefixModes icm)
       ||             m `elem` view modesLists icm ->
           let (arg,args') =
                    case args of
                      []   -> (Text.empty,[])
                      x:xs -> (x,xs)
           in cons (polarity,m,arg) <$> computeMode polarity ms args'

        | not polarity && m `elem` view modesSetArg icm
       ||                 m `elem` view modesNeverArg icm ->
           do res <- computeMode polarity ms args
              return ((polarity,m,Text.empty) : res)

        | otherwise -> Nothing

-- | Construct the arguments to a MODE command corresponding to the given
-- mode changes.
unsplitModes ::
  [(Bool,Char,Text)] {- ^ (set,mode,parameter) -} ->
  [Text]
unsplitModes modes
  = Text.pack (foldr combineModeChars (const "") modes True)
  : args
  where
  args = [arg | (_,_,arg) <- modes, not (Text.null arg)]
  combineModeChars (q,m,_) rest p
    | p == q    =       m : rest p
    | q         = '+' : m : rest True
    | otherwise = '-' : m : rest False
