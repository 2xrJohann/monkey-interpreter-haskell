module Lexer.Token where

data Token =
    ILLEGAL
    | EOF

    {- Operators -}
    | ASSIGN
    | PLUS
    | MINUS
    | BANG
    | ASTERISK
    | SLASH
    | LT
    | GT
    | EQUAL
    | NOTEQUAL

    {- Delims -}
    | COMMA
    | SEMICOLON

    {- Keywords -}
    | FUNCTION
    | LET

    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE

    {- Identifiers & Literals -}
    | INTEGER Int
    | IDENT String

    {- Booleans -}
    | TRUE
    | FALSE

    {- Control Flow -}
    | IF
    | ELSE
    | RETURN
    deriving (Eq, Show)

showToken :: Token -> String
showToken token = case token of
    ILLEGAL        -> "ILLEGAL"
    EOF            -> "EOF"
    ASSIGN         -> "ASSIGN"
    PLUS           -> "PLUS"
    MINUS          -> "MINUS"
    BANG           -> "BANG"
    ASTERISK       -> "ASTERISK"
    SLASH          -> "SLASH"
    Lexer.Token.LT -> "LT"
    Lexer.Token.GT -> "GT"
    EQUAL          -> "EQUAL"
    NOTEQUAL       -> "NOTEQUAL"
    COMMA          -> "COMMA"
    SEMICOLON      -> "SEMICOLON"
    FUNCTION       -> "FUNCTION"
    LET            -> "LET"
    LPAREN         -> "LPAREN"
    RPAREN         -> "RPAREN"
    LBRACE         -> "LBRACE"
    RBRACE         -> "RBRACE"
    INTEGER _      -> "INTEGER"
    IDENT _        -> "IDENT"
    TRUE           -> "TRUE"
    FALSE          -> "FALSE"
    IF             -> "IF"
    ELSE           -> "ELSE"
    RETURN         -> "RETURN"
