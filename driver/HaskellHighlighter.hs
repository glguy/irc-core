module HaskellHighlighter (highlightHaskell) where

import Language.Haskell.Lexer

highlightHaskell :: String -> String
highlightHaskell src = colorize =<< lexerPass0 src

colorize :: PosToken -> String
colorize (tok, (_,str)) =
  case tok of
    Varid    -> str
    Conid    -> yellow str
    Varsym   -> lightgreen str
    Consym   -> lightblue str
    Reservedid -> lightgreen str
    Reservedop -> lightgreen str
    Specialid  -> str
    IntLit     -> lightgreen str
    FloatLit   -> lightgreen str
    CharLit    -> lightgreen str
    StringLit  -> lightgray str
    Qvarid     -> str
    Qconid     -> yellow str
    Qvarsym    -> lightgreen str
    Qconsym    -> lightblue str
    Special    -> red str
    Whitespace -> str
    NestedCommentStart -> gray str
    NestedComment -> gray str
    LiterateComment -> gray str
    Commentstart -> gray str
    Comment -> gray str
    ErrorToken -> str
    GotEOF -> str
    ModuleName -> yellow str
    ModuleAlias -> lightgray str
    Layout -> str
    Indent _ -> str
    Open _ -> str
    TheRest -> str

yellow x = "\03\&07" ++ x ++ "\03\02\02"
lightgreen  x = "\03\&03" ++ x ++ "\03\02\02"
red    x = "\03\&04" ++ x ++ "\03\02\02"
lightblue x = "\03\&10" ++ x ++ "\03\02\02"
gray x = "\03\&14" ++ x ++ "\03\02\02"
lightgray x = "\03\&15" ++ x ++ "\03\02\02"
