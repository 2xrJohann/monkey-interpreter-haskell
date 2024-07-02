module Lexer.Lexer where
import qualified Lexer.Token as Token
import qualified Lexer.Keywords as Keywords
import Data.Char
import Prelude hiding (lex)

data T = T {
    input :: String,
    ch :: Maybe Char
} deriving Show

new :: String -> T

new "" = T { input = "", ch = Nothing}

new input = T { input = input, ch = Just $ head input}

next :: T -> T

next lexer = case input lexer of
    "" -> T { input = "", ch = Nothing}
    _ -> let remainder = tail $ input lexer in
        if null remainder then
        T { input = "", ch = Nothing } else
        T { input = remainder, ch = Just $ head remainder }

lex :: T -> (T, Token.Token)

lex lexer =
    case fmap processChar (ch lexer) of
    Nothing -> (lexer, Token.EOF)
    Just (nextLexer, token) -> (nextLexer, token)
    where
    processChar char =
        case char of
        x | isSpace x -> lex $ next lexer
        '=' -> peekEquals lexer Token.ASSIGN Token.EQUAL
        '!' -> peekEquals lexer Token.BANG Token.NOTEQUAL
        '+' -> (next lexer, Token.PLUS)
        '-' -> (next lexer, Token.MINUS)
        '*' -> (next lexer, Token.ASTERISK)
        '/' -> (next lexer, Token.SLASH)
        '<' -> (next lexer, Token.LT)
        '>' -> (next lexer, Token.GT)
        ',' -> (next lexer, Token.COMMA)
        ';' -> (next lexer, Token.SEMICOLON)
        '(' -> (next lexer, Token.LPAREN)
        ')' -> (next lexer, Token.RPAREN)
        '{' -> (next lexer, Token.LBRACE)
        '}' -> (next lexer, Token.RBRACE)
        x | isDigit x -> let (int, newLexer) = readChars lexer isDigit
            in (newLexer, Token.INTEGER $ read int)
        x | isLetter x -> let (ident, newLexer) = readChars lexer isLetter
            in (newLexer, Keywords.lookupKeyword ident)
        x -> (next lexer, Token.ILLEGAL)

    readChars :: T -> (Char -> Bool) -> (String, T)

    readChars lexer op =
        let readChars' lexer buffer =
                case ch lexer of
                Nothing -> (reverse buffer, lexer)
                Just character ->
                    if op character
                    then
                        readChars' (next lexer) (character : buffer)
                    else
                        (reverse buffer, lexer)
        in readChars' lexer []

    peekEquals :: T -> Token.Token -> Token.Token -> (T, Token.Token)

    peekEquals lexer default' peek =
        maybe (nextLexer, default') peekEquals' (ch nextLexer)
        where
            nextLexer = next lexer

            peekEquals' character =
                case character of
                '=' -> (next nextLexer, peek)
                _ -> (nextLexer, default')
