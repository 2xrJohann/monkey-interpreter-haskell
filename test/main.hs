module Main where

import qualified Lexer.Token as Token
import qualified Lexer.Lexer as Lexer

data Test = Test {
    expectedType :: Token.Token,
    tokenString :: String
}

main :: IO ()

main = do 
    print "hello"
    runTests
    where

    testInput :: String
    testInput = "5=+hello(){  } , ;let 7 834 != == ! "

    initedLexer :: Lexer.T
    initedLexer = Lexer.new testInput

    newTest :: Token.Token -> String -> Test
    newTest eT tS =
        Test {
            expectedType = eT,
            tokenString = tS
        }
    
    testIO :: [Test]
    testIO = [
        newTest (Token.INTEGER 5) "5",
        newTest Token.ASSIGN "=",
        newTest Token.PLUS "+",
        newTest (Token.IDENT "hello") "Ident: Hello",
        newTest Token.LPAREN "(",
        newTest Token.RPAREN ")",
        newTest Token.LBRACE "{",
        newTest Token.RBRACE "}",
        newTest Token.COMMA ",",
        newTest Token.SEMICOLON ";",
        newTest Token.LET "let",
        newTest (Token.INTEGER 7) "7",
        newTest (Token.INTEGER 834) "834",
        newTest Token.NOTEQUAL "not equal",
        newTest Token.EQUAL "equal",
        newTest Token.BANG "!",
        newTest Token.EOF "EOF"
     ]

    runTest :: Token.Token -> Test -> String
    runTest tokenType test =
        let expected = expectedType test
        in if expected == tokenType
        then "Pass"
        else "Fail got: " ++ show tokenType
    
    runTests :: IO ()
    runTests =
            let (nextLexer, token) = Lexer.lex initedLexer in
            let runTests' token lexer tests =
                    case tests of
                        [] -> []
                        (test : remaining) ->
                            let (nextLexer, nextToken) = Lexer.lex lexer
                            in runTest token test : runTests' nextToken nextLexer remaining
            in let res = runTests' token nextLexer testIO
            in mapM_ (\(test, res) -> putStrLn $ tokenString test ++ " : " ++ res) $ zip testIO res
