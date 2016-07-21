{-# Language TemplateHaskell #-}
{-# Language BangPatterns #-}
module Irc.Modes where

import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as Text

-- | Settings that describe how to interpret channel modes
data ModeTypes = ModeTypes
  { _modesLists       :: ![Char]
  , _modesAlwaysArg   :: ![Char]
  , _modesSetArg      :: ![Char]
  , _modesNeverArg    :: ![Char]
  , _modesPrefixModes :: ![(Char,Char)]
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

-- | The default UMODE as defined by Freenode
defaultUmodeTypes :: ModeTypes
defaultUmodeTypes = ModeTypes
  { _modesLists     = ""
  , _modesAlwaysArg = ""
  , _modesSetArg    = "s"
  , _modesNeverArg  = ""
  , _modesPrefixModes = []
  }

-- | Split up a mode change command and arguments into individual changes
-- given a configuration.
splitModes ::
  ModeTypes {- ^ mode interpretation -} ->
  Text      {- ^ modes               -} ->
  [Text]    {- ^ arguments           -} ->
  Maybe [(Bool,Char,Text)]
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

unsplitModes :: [(Bool,Char,Text)] -> [Text]
unsplitModes modes
  = Text.pack (foldr combineModeChars (const "") modes True)
  : args
  where
  args = [arg | (_,_,arg) <- modes, not (Text.null arg)]
  combineModeChars (q,m,_) rest p
    | p == q    =       m : rest p
    | q         = '+' : m : rest True
    | otherwise = '-' : m : rest False
