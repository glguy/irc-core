module HaskellHighlighter (highlightHaskell, highlightType) where

import Data.List (intersperse)
import Language.Haskell.Lexer
import Language.Haskell.Exts.Parser (parseWithMode, ParseMode(..), ParseResult(..))
import Language.Haskell.Exts.Syntax (Asst(..), Context, TyVarBind(..), Type(..), Boxed(..),QName(Qual,UnQual), Name(..))
import Language.Haskell.Exts.Extension (Extension(..), Language(Haskell2010), KnownExtension(..))
import Language.Haskell.Exts.Pretty (Pretty, prettyPrintStyleMode, Style(mode), style, Mode(OneLineMode), defaultMode)

highlightHaskell :: String -> String
highlightHaskell src = init (colorize (lexerPass0 (src++"\n")))
-- the lexer requires this newline for single-line comments to work

colorize :: [PosToken] -> String
colorize [] = ""
colorize ((_,(_,"`")):(Varid,(_,str)):(_,(_,"`")):rest)
  = orange ("`" ++ str ++ "`") ++ colorize rest
colorize ((tok, (_,str)):rest) = aux str ++ colorize rest
  where
  aux =
    case tok of
      Varid              -> id
      Conid              -> id
      Varsym             -> orange
      Consym             -> orange
      Reservedid ->
        case str of
          "case"         -> orange
          "of"           -> orange
          "do"           -> orange
          "if"           -> orange
          "then"         -> orange
          "else"         -> orange
          "let"          -> orange
          "in"           -> orange

          "import"       -> pink
          "infixl"       -> pink
          "infixr"       -> pink
          "infix"        -> pink

          "_"            -> id
          _              -> green

      Reservedop         -> orange
      Specialid          -> id
      IntLit             -> red
      FloatLit           -> red
      CharLit            -> red
      StringLit          -> red
      Qvarid             -> id
      Qconid             -> id
      Qvarsym            -> orange
      Qconsym            -> orange
      Special            -> id
      Whitespace         -> id
      NestedCommentStart -> comment
      NestedComment      -> comment
      LiterateComment    -> comment
      Commentstart       -> comment
      Comment            -> comment
      ErrorToken         -> id
      GotEOF             -> id
      ModuleName         -> id
      ModuleAlias        -> id
      Layout             -> id
      Indent _           -> id
      Open _             -> id
      TheRest            -> id

comment :: String -> String
comment = cyan

green, orange, red, cyan, pink, purple :: String -> String
green  x = "\03\&03" ++ x ++ "\03\02\02"
orange x = "\03\&07" ++ x ++ "\03\02\02"
red    x = "\03\&04" ++ x ++ "\03\02\02"
cyan   x = "\03\&11" ++ x ++ "\03\02\02"
pink   x = "\03\&13" ++ x ++ "\03\02\02"
purple x = "\03\&06" ++ x ++ "\03\02\02"

data Polarity = P | N | I | G | U
invert :: Polarity -> Polarity
invert P = N
invert N = P
invert I = I
invert G = G
invert U = U

highlightType :: String -> Maybe String
highlightType = fmap renderType . myParseType

renderType :: Type -> String
renderType t = renderTypeS P t ""

renderTypeS :: Polarity -> Type -> ShowS
renderTypeS polarity t0 =
  case t0 of
    TyForall vars cxt t ->
         maybe id renderVars vars
       . renderCxt (invert polarity) cxt
       . renderTypeS polarity t

    TyFun f x ->
         renderTypeS (invert polarity) f
       . showString (orange " -> ")
       . renderTypeS polarity x

    TyTuple boxed ts ->
         showTuple boxed
           (showCommaSep
             (map (renderTypeS polarity) ts))

    TyList t ->
         showChar '['
       . renderTypeS polarity t
       . showChar ']'

    TyParArray t ->
         showString "[: "
       . renderTypeS polarity t
       . showString " :]"

    -- TODO: much smarter
    TyCon n -> showString (polarityColor polarity (myPretty n))
    TyInfix l n r ->
         renderTypeS U l
       . showChar ' '
       . showString (polarityColor polarity (prettyOperator n))
       . showChar ' '
       . renderTypeS U r
    TyApp f x ->
         renderTypeS polarity f
       . showChar ' '
       . renderTypeS U x

    TyVar v -> showString (polarityColor polarity (myPretty v))

    TyKind t k ->
         showString "("
       . renderTypeS polarity t
       . showString " :: "
       . showString (myPretty k)
       . showString ")"

    TyParen t ->
         showChar '('
       . renderTypeS polarity t
       . showChar ')'

    TyEquals l r ->
         renderTypeS U l
       . showString (polarityColor polarity " ~ ")
       . renderTypeS U r

    TyPromoted p -> showString (myPretty p) -- TODO
    TySplice s -> showString (myPretty s) -- TODO
    TyBang _ t -> renderTypeS polarity t -- shouldn't occur...

renderVars :: [TyVarBind] -> ShowS
renderVars vars
  = showString "forall"
  . showString (concatMap ((' ':). myPretty) vars)
  . showString ". "

renderCxt :: Polarity -> Context -> ShowS
renderCxt _ [] = id
renderCxt polarity [x]
  = showConstraint polarity x
  . showString (orange " => ")
renderCxt polarity xs
  = showString "("
  . showCommaSep (map (showConstraint polarity) xs)
  . showString (") " ++ orange "=> ")

showConstraint :: Polarity -> Asst -> ShowS
showConstraint polarity (ClassA n ts)
  = showString (polarityColor polarity (myPretty n))
  . showString (concatMap (\x -> ' ':renderTypeS U x "") ts)
showConstraint polarity (VarA n)
  = showString (polarityColor polarity (myPretty n))
showConstraint polarity (InfixA l n r)
  = renderTypeS U l
  . showString (polarityColor polarity (prettyOperator n))
  . renderTypeS U r
showConstraint polarity (IParam n t)
  = showString "("
  . showString (myPretty n)
  . showString " :: "
  . renderTypeS polarity t
  . showString ")"
showConstraint polarity (EqualP l r)
  = renderTypeS U l
  . showString (polarityColor polarity " ~ ")
  . renderTypeS U r

showConstraint polarity (ParenA c)
  = showChar '('
  . showConstraint polarity c
  . showChar ')'

prettyOperator :: QName -> String
prettyOperator (Qual _mod n) = prettyOperatorName n -- TODO
prettyOperator (UnQual n) = prettyOperatorName n
prettyOperator _ = "unsupported"

prettyOperatorName :: Name -> String
prettyOperatorName (Ident x) = "`"++x++"`"
prettyOperatorName (Symbol x) = x

showTuple :: Boxed -> ShowS -> ShowS
showTuple Unboxed s = showString "(# " . s . showString " #)"
showTuple Boxed   s = showString "(" . s . showString ")"

showCommaSep :: [ShowS] -> ShowS
showCommaSep = foldr (.) id . intersperse (showChar ',')

polarityColor :: Polarity -> String -> String
polarityColor P = green
polarityColor N = red
polarityColor I = purple
polarityColor G = cyan
polarityColor U = id

myParseType :: String -> Maybe Type
myParseType str =
  case parseWithMode myParseMode str of
    ParseOk x -> Just x
    ParseFailed{} -> Nothing

myParseMode :: ParseMode
myParseMode = ParseMode
  { parseFilename = "command"
  , baseLanguage  = Haskell2010
  , extensions    = map EnableExtension
                  [ MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies,
                    TypeOperators, UnboxedTuples, DataKinds, ConstraintKinds ]
  , ignoreLanguagePragmas = True
  , ignoreLinePragmas = True
  , fixities = Nothing
  }

myPretty :: Pretty a => a -> String
myPretty = prettyPrintStyleMode style { mode = OneLineMode } defaultMode

knownTypePolarities :: [(Name, [Polarity])]
knownTypePolarities = []
