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
