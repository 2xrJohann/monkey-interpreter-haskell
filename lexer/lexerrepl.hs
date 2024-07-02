module LexerREPL where

import qualified Lexer.Lexer as Lexer
import qualified Lexer.Token as Token

{- Compile REPL with `ghc lexer/lexerrepl.hs -main-is LexerREPL` -}

main = do
    print "Welcome to monkey lexer REPL"
    inputLoop
    where
        prompt = "λ "

        inputLoop = do
            print prompt
            input <- getLine
            if input == "quit"
                then putStrLn "goodbye"
                else do
                    yieldUntilEOF $ Lexer.new input

        yieldUntilEOF lexer =
            let (nextLexer, token) = Lexer.lex lexer in
            if token == Token.EOF 
            then inputLoop
            else do 
                print token 
                yieldUntilEOF nextLexer