{-# Language OverloadedStrings #-}
module Client.Mask
  ( Mask
  , matchMask
  , buildMask
  ) where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Irc.Identifier (Identifier, idTextNorm, mkId)
import Irc.UserInfo (UserInfo, renderUserInfo)
import Text.Regex.TDFA
import Text.Regex.TDFA.String (compile)

newtype Mask = Mask Regex

-- | Compile a list of masks down to a single, reuseable 'Mask' value
-- suitable for being used with 'matchMask'.
--
-- Masks can match zero-to-many arbitrary characters with @*@.
--
-- Masks can match one arbitrary character with @?@.
--
-- Literal @*@ @?@ and @|@ can be matched with a preceding @\@.
--
-- Missing host or username components of a mask will automatically
-- be treated as wildcards.
buildMask ::
  [Identifier] {- ^ masks -} ->
  Mask
buildMask patterns =
  case componentsToMask (map (translate . parseMaskComponents . idTextNorm) patterns) of
    Left e -> error e
    Right m -> m

-- | Determine if a given 'Mask' matches a given 'UserInfo'
matchMask :: Mask -> UserInfo -> Bool
matchMask (Mask re) userInfo =
  matchTest re (Text.unpack (normalized (renderUserInfo userInfo)))

normalized :: Text -> Text
normalized = idTextNorm . mkId

-- | Parse a mask into the nick, user, and hostname components
-- while replacing omitted components with @"*"@.
parseMaskComponents :: Text -> String
parseMaskComponents str = Text.unpack nick ++ "!" ++ user ++ "@" ++ host
  where
    (nickuser,rawhost) = Text.break (=='@') str
    (nick    ,rawuser) = Text.break (=='!') nickuser

    user = defaultWild rawuser
    host = defaultWild rawhost

    defaultWild x =
      case Text.uncons x of
        Nothing     -> "*"
        Just (_, y) -> Text.unpack y

componentsToMask :: [String] -> Either String Mask
componentsToMask xs =
  Mask <$> compile defaultCompOpt { multiline     = False }
                   defaultExecOpt { captureGroups = False }
                   ("^(" ++ intercalate "|" xs ++ ")$")

-- | Translate from the languge of masks to the language of
-- regular expressions.
--
-- Masks support the @*@ (many) and @?@ (one) wildcards. Wildcards
-- and @\@ can be escaped by preceding them with a @\@. All other
-- uses of @\@ are treated as matching the literal backslash.
translate :: String -> String
translate [] = []
translate ('\\' : '*'  : xs) = '\\' : '*'  : translate xs
translate ('\\' : '?'  : xs) = '\\' : '?'  : translate xs
translate ('\\' : '\\' : xs) = '\\' : '\\' : translate xs
translate ('*'         : xs) = '.'  : '*'  : translate xs
translate ('?'         : xs) = '.'  : '?'  : translate xs
translate (x           : xs)
  | isMetaChar x = '\\' : x : translate xs
  | otherwise    =        x : translate xs

-- | returns True iff the charactr is a regular expression meta character:
-- @^$\\.|*?+()[]{}@
isMetaChar :: Char -> Bool
isMetaChar c = case c of
  '^'  -> True
  '\\' -> True
  '.'  -> True
  '|'  -> True
  '*'  -> True
  '?'  -> True
  '+'  -> True
  '('  -> True
  ')'  -> True
  '['  -> True
  ']'  -> True
  '{'  -> True
  '}'  -> True
  '$'  -> True
  _    -> False
