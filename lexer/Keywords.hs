module Lexer.Keywords where

import qualified Lexer.Token as Token

data Keywords =
    FN
    | LET

lookupKeyword :: String -> Token.Token

lookupKeyword keyword =
    case keyword of
    "fn" -> Token.FUNCTION
    "let" -> Token.LET
    "true" -> Token.TRUE
    "false" -> Token.FALSE
    "if" -> Token.IF
    "else" -> Token.ELSE
    "return" -> Token.RETURN
    ident -> Token.IDENT ident
