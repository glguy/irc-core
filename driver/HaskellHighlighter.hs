module HaskellHighlighter (highlightHaskell) where

import Language.Haskell.Lexer

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
      Varid    -> id
      Conid    -> id
      Varsym   -> orange
      Consym   -> orange
      Reservedid ->
        case str of
          "case" -> orange
          "of"   -> orange
          "do"   -> orange
          "if"   -> orange
          "else" -> orange
          "let"  -> orange
          "in"   -> orange

          "import" -> pink
          "infixl" -> pink
          "infixr" -> pink
          "infix"  -> pink

          "_" -> id
          _ -> green
      Reservedop -> orange
      Specialid  -> id
      IntLit     -> red
      FloatLit   -> red
      CharLit    -> red
      StringLit  -> red
      Qvarid     -> id
      Qconid     -> id
      Qvarsym    -> orange
      Qconsym    -> orange
      Special    -> id
      Whitespace -> id
      NestedCommentStart -> gray
      NestedComment -> gray
      LiterateComment -> gray
      Commentstart -> gray
      Comment -> gray
      ErrorToken -> gray
      GotEOF -> id
      ModuleName -> id
      ModuleAlias -> id
      Layout -> id
      Indent _ -> id
      Open _ -> id
      TheRest -> id

green, orange, red, gray :: String -> String
green  x = "\03\&03" ++ x ++ "\03\02\02"
orange x = "\03\&07" ++ x ++ "\03\02\02"
red    x = "\03\&04" ++ x ++ "\03\02\02"
gray   x = "\03\&14" ++ x ++ "\03\02\02"
pink   x = "\03\&13" ++ x ++ "\03\02\02"
