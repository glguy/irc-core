module HaskellHighlighter (highlightHaskell, highlightType) where

import Data.List (foldl',intersperse)
import Data.Monoid
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Language.Haskell.Lexer
import Language.Haskell.Exts.Parser (parseWithMode, ParseMode(..), ParseResult(..))
import Language.Haskell.Exts.Syntax (Asst(..), Context, TyVarBind(..), Type(..), Boxed(..),QName(Qual,UnQual), Name(..))
import Language.Haskell.Exts.Extension (Extension(..), Language(Haskell2010), KnownExtension(..))
import Language.Haskell.Exts.Pretty (Pretty, prettyPrintStyleMode, Style(mode), style, Mode(OneLineMode), defaultMode)
import qualified Data.Map as Map

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
  deriving (Show,Read, Eq)

invert :: Polarity -> Polarity
invert P = N
invert N = P
invert I = I
invert G = G
invert U = U

highlightType :: String -> Maybe String
highlightType = fmap renderType . myParseType

renderType :: Type -> String
renderType t = renderTypeS P knownTypePolarities t ""

renderTypeS :: Polarity -> Map Name (Maybe [Polarity]) -> Type -> ShowS
renderTypeS polarity facts t0 =
  case t0 of
    TyForall vars cxt t ->
         maybe id renderVars vars
       . renderCxt (invert polarity) cxt
       . renderTypeS polarity facts' t
      where
      vars'  = fromMaybe [] vars
      facts' = foldl' (flip learnFacts)
                 (foldl' (flip Map.delete) facts (map tyVarName vars'))
                 cxt

    TyFun f x ->
         renderTypeS (invert polarity) facts f
       . showString (orange " -> ")
       . renderTypeS polarity facts x

    TyTuple boxed ts ->
         showTuple boxed
           (showCommaSep
             (map (renderTypeS polarity facts) ts))

    TyList t ->
         showChar '['
       . renderTypeS polarity facts t
       . showChar ']'

    TyParArray t ->
         showString "[: "
       . renderTypeS polarity facts t
       . showString " :]"

    -- TODO: much smarter
    TyCon n -> showString (polarityColor polarity (myPretty n))
    TyInfix l n r ->
         renderTypeS U facts l
       . showChar ' '
       . showString (polarityColor polarity (prettyOperator n))
       . showChar ' '
       . renderTypeS U facts r

    TyApp f x ->
         renderTyApp polarity facts (TyApp f x)

    TyVar v -> showString (polarityColor polarity (myPretty v))

    TyKind t k ->
         showString "("
       . renderTypeS polarity facts t
       . showString " :: "
       . showString (myPretty k)
       . showString ")"

    TyParen t ->
         showChar '('
       . renderTypeS polarity facts t
       . showChar ')'

    TyEquals l r ->
         renderTypeS U facts l
       . showString (polarityColor polarity " ~ ")
       . renderTypeS U facts r

    TyPromoted p -> showString (myPretty p) -- TODO
    TySplice s -> showString (myPretty s) -- TODO
    TyBang _ t -> renderTypeS polarity facts t -- shouldn't occur...

renderTyApp :: Polarity -> Map Name (Maybe [Polarity]) -> Type -> ShowS
renderTyApp polarity facts t0 =
  case appsToList t0 of
    f:xs ->
         renderTypeS polarity facts f
       . foldr (.) id
           (zipWith doOne info xs)
      where
      pickPolarity U = U
      pickPolarity I = I
      pickPolarity G = G
      pickPolarity P = polarity
      pickPolarity N = invert polarity

      doOne p t = showChar ' '
                . renderTypeS (pickPolarity p) facts t

      info = case f of
               TyVar n          -> info1 n
               TyCon (UnQual n) -> info1 n
               _                -> repeat U

      info1 n = case Map.lookup n facts of
                  Just (Just ps) -> ps
                  _              -> repeat U

    _ -> error "renderTyApp: bad type argument"


-- This is making a bare-minimum effort
appsToList :: Type -> [Type]
appsToList (TyApp f x) = appsToList f ++ [x]
appsToList x           = [x]

renderVars :: [TyVarBind] -> ShowS
renderVars vars
  = showString "forall"
  . showString (concatMap ((' ':). myPretty) vars)
  . showString ". "

tyVarName :: TyVarBind -> Name
tyVarName (KindedVar n _) = n
tyVarName (UnkindedVar n) = n

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
  . showString (concatMap (\x -> ' ':renderTypeS U mempty x "") ts)
showConstraint polarity (VarA n)
  = showString (polarityColor polarity (myPretty n))
showConstraint polarity (InfixA l n r)
  = renderTypeS U mempty l
  . showString (polarityColor polarity (prettyOperator n))
  . renderTypeS U mempty r
showConstraint polarity (IParam n t)
  = showString "("
  . showString (myPretty n)
  . showString " :: "
  . renderTypeS polarity mempty t
  . showString ")"
showConstraint polarity (EqualP l r)
  = renderTypeS U mempty l
  . showString (polarityColor polarity " ~ ")
  . renderTypeS U mempty r

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
showCommaSep = foldr (.) id . intersperse (showString ", ")

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

learnFacts :: Asst -> Map Name (Maybe [Polarity]) -> Map Name (Maybe [Polarity])
learnFacts (ClassA (UnQual klass) [TyVar t]) facts
  | Just (Just xs) <- Map.lookup klass facts =
      Map.insertWith mergeFactPolarity t (Just xs) facts
learnFacts _ facts = facts -- TODO: invalidate stuff?

mergeFactPolarity :: Maybe [Polarity] -> Maybe [Polarity] -> Maybe [Polarity]
mergeFactPolarity mbNew mbOld =
  do new <- mbNew
     old <- mbOld
     return (zipWith mergeFactPolarity1 new old)

mergeFactPolarity1 :: Polarity -> Polarity -> Polarity
mergeFactPolarity1 P N = I
mergeFactPolarity1 N P = I
mergeFactPolarity1 G x = x
mergeFactPolarity1 x G = x
mergeFactPolarity1 I _ = I
mergeFactPolarity1 _ I = I
mergeFactPolarity1 x y | x == y = x
mergeFactPolarity1 _ _ = U

knownTypePolarities :: Map Name (Maybe [Polarity])
knownTypePolarities =
  Map.fromList
   [(Ident "Functor",Just [P])
   ,(Ident "Applicative",Just [P])
   ,(Ident "Monad",Just [P])
   ,(Ident "Comonad",Just [P])
   ,(Ident "Contravariant",Just [N])
   ,(Ident "Profunctor", Just [N,P])
   ,(Ident "Bifunctor", Just [P,P])
   ,(Ident "Endo", Just [I])
   ,(Ident "Proxy", Just [G])
   ,(Ident "Const", Just [P,G])
   ,(Ident "Identity", Just [P])
   ,(Ident "Settable", Just [P])
   ,(Ident "Traversable", Just [P])
   ,(Ident "Foldable", Just [P])
   ,(Ident "Choice", Just [N,P])

   ,(Ident "Setter", Just [N,P,P,N])
   ,(Ident "Prism", Just [N,P,P,N])
   ,(Ident "Lens", Just [N,P,P,N])
   ,(Ident "Traversal", Just [N,P,P,N])
   ,(Ident "Iso", Just [N,P,P,N])

   ,(Ident "Setter'", Just [I,I])
   ,(Ident "Lens'", Just [I,I])
   ,(Ident "Prism'", Just [I,I])
   ,(Ident "Traversal'", Just [I,I])
   ,(Ident "Iso'", Just [I,I])

   ,(Ident "Getter", Just [N,P])
   ,(Ident "Fold", Just [N,P])
   ,(Ident "Review", Just [P,N])
   ]
