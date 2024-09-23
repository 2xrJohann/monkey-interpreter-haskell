module Parser where

import Lexer.Lexer as Lexer
import qualified Lexer.Token as Token
import Data.Map (Map)
import qualified Data.Map as Map
import Ast
import System.IO
import Control.Monad (forever)

data Parser = Parser {
    lexer :: Lexer.T,
    currToken :: Token.Token,
    peekToken :: Token.Token,
    prefixParseFns :: Map String (Parser -> (Parser, Ast.Expression)),
    infixParseFns :: Map String (Parser -> Ast.Expression -> (Parser, Ast.Expression)),
    errors :: [String]
}

errorHelper :: Either Parser String -> Parser -> Parser
errorHelper wrapper parser =
    case wrapper of
    Left p -> p
    Right err ->
        let _ = error ("ERROR IN ERROR HELPER: " ++ err) in
        parser { errors = err : errors parser}

buildPrefixParseFns :: Map String (Parser -> (Parser, Ast.Expression))
buildPrefixParseFns =
    Map.fromList $
        [
            (Token.showToken $ Token.IDENT "", parseIdentifier),
            (Token.showToken $ Token.INTEGER 0, parseIntegerLiteral),
            (Token.showToken Token.LPAREN, parseGroupedExpression),
            (Token.showToken Token.IF, parseIfExpression),
            (Token.showToken Token.FUNCTION, parseFunctionLiteral)
        ] ++
        [   (Token.showToken token, parseBoolean) | token <- [Token.TRUE, Token.FALSE]
        ] ++
        [   (Token.showToken token, parsePrefixExpression) | token <- [Token.BANG, Token.MINUS] ]

buildInfixParseFns :: Map String (Parser -> Ast.Expression -> (Parser, Ast.Expression))
buildInfixParseFns =
    let tokenList = [Token.PLUS, Token.MINUS, Token.ASTERISK, Token.SLASH, Token.EQUAL, Token.NOTEQUAL, Token.LT, Token.GT] in
    Map.fromList $ mapTokens tokenList ++ [(Token.showToken Token.LPAREN, parseCallExpression)]
    where
        mapTokens = map (\token -> (Token.showToken token, parseInfixExpression))

parseCallExpression :: Parser -> Expression -> (Parser, Ast.Expression)
parseCallExpression parser func =
    let next = nextParser parser in
    if peekToken parser == Token.RPAREN
        then
            (next, CALLEXPRESSION $ CallExpression func [])
        else
        let (parser, arg) = parseExpression next LOWEST in
        parseCallExpression' parser [arg]
    where
    parseCallExpression' parser acc =
        if peekToken parser == Token.RPAREN
            then
                (nextParser parser, CALLEXPRESSION $ CallExpression func (reverse acc))
        else
            let next = nextParser $ nextParser parser in
            let (parser, arg) = parseExpression next LOWEST in
            parseCallExpression' parser (arg : acc)

parseIfExpression :: Parser -> (Parser, Ast.Expression)
parseIfExpression parser =
    let lparen = errorHelper (expectPeek parser Token.LPAREN) parser in
    let next = nextParser lparen in
    let (exParser, condition) = parseExpression next LOWEST in
    let rparen = errorHelper (expectPeek exParser Token.RPAREN) exParser in
    let lbrace = errorHelper (expectPeek rparen Token.LBRACE) rparen in
    let (blockParser, consequence) = parseBlockStatement lbrace in
    if peekToken blockParser == Token.ELSE
        then
            let next = nextParser blockParser in
            let lbrace = errorHelper (expectPeek next Token.LBRACE) next in
            let (returnParser, alternative) = parseBlockStatement lbrace in
            (returnParser, Ast.IFEXPRESSION $ IfExpression condition consequence (Just alternative))
        else
            (blockParser, Ast.IFEXPRESSION $ IfExpression condition consequence Nothing)

parseFunctionLiteral :: Parser -> (Parser, Expression)
parseFunctionLiteral parser =
    let lparen = errorHelper (expectPeek parser Token.LPAREN) parser in
    let (next, params) = parseFunctionParameters $ nextParser lparen in
    let lbrace = errorHelper (expectPeek next Token.LBRACE) next in
    let (retParser, body) = parseBlockStatement lbrace in
    (retParser, FUNCTIONLITERAL $ FunctionLiteral params body)

parseFunctionParameters :: Parser -> (Parser, [Ast.Expression])
parseFunctionParameters parser =
    let a = error "bitch ENDS ON: " ++ show (currToken parser) in
    parseFunctionParameters' parser []
    where
    parseFunctionParameters' parser acc =
        case currToken parser of
            Token.RPAREN -> (parser, reverse acc)
            Token.COMMA -> parseFunctionParameters' (nextParser parser) acc
            otherwise -> let (next, ident) = parseIdentifier parser in
                parseFunctionParameters' (nextParser next) (ident : acc)

parseBlockStatement :: Parser -> (Parser, Ast.BlockStatement)
parseBlockStatement parser =
    let next = nextParser parser in
    parseBlockStatement' next []
    where
    parseBlockStatement' parser acc =
        let token = currToken parser in
        if token /= Token.RBRACE && token /= Token.EOF
            then
                let (statement, next) = parseStatement parser in
                    parseBlockStatement' (nextParser next) (statement : acc)
            else
                (parser, Ast.BlockStatement $ reverse acc)


parseGroupedExpression :: Parser -> (Parser, Ast.Expression)
parseGroupedExpression parser =
    let next = nextParser parser in
    let (np, ex) = parseExpression next LOWEST in
    let retParser = errorHelper (expectPeek np Token.RPAREN) np in
        (retParser, ex)


parseInfixExpression :: Parser -> Expression -> (Parser, Expression)
parseInfixExpression parser infixLeft =
    let infixOperator = Token.showToken $ currToken parser in
    let precedence = currPrecedence parser in
    let next = nextParser parser in
    let (retParser, infixRight) = parseExpression next precedence in
    (retParser, Ast.INFIXEXPRESSION $ Ast.InfixExpression infixLeft infixOperator infixRight)

parseBoolean :: Parser -> (Parser, Ast.Expression)
parseBoolean parser =
    case currToken parser of
        Token.TRUE -> (parser, Ast.BOOLEAN True)
        Token.FALSE -> (parser, Ast.BOOLEAN False)
        otherwise -> error "expected bool"

parseIdentifier :: Parser -> (Parser, Ast.Expression)
parseIdentifier parser =
    case currToken parser of
        Token.IDENT x ->  (parser, Ast.IDENTIFIER x)
        otherwise -> error ("expected ident, got " ++ show  (currToken parser))

parseIntegerLiteral :: Parser -> (Parser, Ast.Expression)
parseIntegerLiteral parser =
    let ct = currToken parser in
    case ct of
        Token.INTEGER x -> (parser, Ast.INTEGER x)
        otherwise -> error "expected int"

new :: Lexer.T -> [String] -> Parser
new lexer errors =
    let (nextLexer, currToken) = Lexer.lex lexer in
    let (_, peekToken) = Lexer.lex nextLexer in
    Parser lexer currToken peekToken buildPrefixParseFns buildInfixParseFns errors

nextParser :: Parser -> Parser
nextParser parser =
    Parser.new (fst $ Lexer.lex $ lexer parser) $ errors parser

expectPeek :: Parser -> Token.Token -> Either Parser String
expectPeek parser expected =
    if expectPeek' parser expected
    then
        Left $ nextParser parser
    else
        error $ show "expected " ++ show expected ++ ", got " ++ show (currToken parser)
    where
    expectPeek' parser expected =
        let pt = peekToken parser in
        case (pt, expected) of
        (Token.IDENT _, Token.IDENT _) -> True
        otherwise -> pt == expected

parseStatement :: Parser -> (Ast.Statement, Parser)
parseStatement parser =
    case currToken parser of
    Token.LET -> parseLetStatement parser
    Token.RETURN -> parseReturnStatement parser
    otherwise -> parseExpressionStatement parser

parseLetStatement :: Parser -> (Ast.Statement, Parser)
parseLetStatement parser =
    let identParser = errorHelper (expectPeek parser $ Token.IDENT "") parser in
    let identStr = case currToken identParser of
            Token.IDENT x -> x
            otherwise -> error "expected ident" in
    let assignParser = errorHelper (expectPeek identParser Token.ASSIGN) identParser in
    let (np, ex) = parseExpression (nextParser assignParser) LOWEST in
    let retParser = if peekToken np == Token.SEMICOLON then nextParser np else np in
    (Ast.LET $ Ast.LetStatement identStr ex, nextParser retParser)


parseReturnStatement :: Parser -> (Statement, Parser)
parseReturnStatement parser =
    let (np, ex) = parseExpression (nextParser parser) LOWEST in
    let retParser = if peekToken np == Token.SEMICOLON then nextParser np else np in
    (RETURN ex, retParser)

data Priority =
    LOWEST
    | EQUALS
    | LESSGREATER
    | SUM
    | PRODUCT
    | PREFIX
    | CALL

prio :: Priority -> Int
prio p =
    case p of
    LOWEST -> 1
    EQUALS -> 2
    LESSGREATER -> 3
    SUM -> 4
    PRODUCT -> 5
    PREFIX -> 6
    CALL -> 7

precedences :: Token.Token -> Priority
precedences token =
    case token of
        Token.EQUAL -> EQUALS
        Token.NOTEQUAL -> EQUALS
        Token.LT -> LESSGREATER
        Token.GT -> LESSGREATER
        Token.PLUS -> SUM
        Token.MINUS -> SUM
        Token.SLASH -> PRODUCT
        Token.ASTERISK -> PRODUCT
        Token.LPAREN -> CALL
        otherwise -> LOWEST

peekPrecedence :: Parser -> Priority
peekPrecedence parser =
    let pt = peekToken parser in
        precedences pt

currPrecedence parser =
    let ct = currToken parser in
        precedences ct

parseExpressionStatement :: Parser -> (Ast.Statement, Parser)
parseExpressionStatement parser =
    let (np, ex) = parseExpression parser LOWEST in
    let retParser = if peekToken np == Token.SEMICOLON then nextParser np else np in
    (Ast.EXPRESSIONSTATEMENT ex, retParser)

parseExpression :: Parser -> Priority -> (Parser, Ast.Expression)
parseExpression parser priority =
    let return = Map.lookup ct prefixFns in
        case return of
        Nothing -> error ("didnt find fn :/ for: " ++ show ct)
        Just fn -> let (next, left) = fn parser in
            parseInfix next priority left
    where
    prefixFns = prefixParseFns parser
    ct = Token.showToken $ currToken parser

parseInfix :: Parser -> Priority -> Ast.Expression -> (Parser, Ast.Expression)
parseInfix parser precedence left
  | peekToken parser == Token.SEMICOLON = (parser, left)
  | prio precedence < prio (peekPrecedence parser) =
    let maybeInfixFnParser = nextParser parser in
    let infixFnOption = Map.lookup (Token.showToken $ currToken maybeInfixFnParser) (infixParseFns parser) in
    case infixFnOption of
        Nothing -> (parser, left)
        Just fn -> let (recParser, right) = fn maybeInfixFnParser left in
            parseInfix recParser precedence right
  | otherwise = (parser, left)

parsePrefixExpression :: Parser -> (Parser, Ast.Expression)
parsePrefixExpression parser =
    let prefixOperator = show $ currToken parser in
    let (next, prefixRight) = parseExpression (nextParser parser) PREFIX in
    (next, PREFIXEXPRESSION $ PrefixExpression prefixOperator prefixRight)

parseProgram :: Parser -> [Statement] -> [Statement]
parseProgram parser stmts =
    if currToken parser == Token.EOF
        then reverse stmts
    else
        let (stmt, next) = parseStatement parser
        in parseProgram (nextParser next) (stmt : stmts)

repl :: IO ()
repl = forever $ do
    putStr "λ type shit >> "
    hFlush stdout
    input <- getLine
    if input == "exit"
        then putStrLn "Exiting..."
        else do
            let testLexer = Lexer.new input
            let tokens = lexAll testLexer
            putStrLn ("tokens -> " ++ show tokens)
            let testParser = Parser.new testLexer []
            let program = parseProgram testParser []
            mapM_ print program

main = do
    repl
    {-let testInput =
         "if(a) {return a;}"
    let testLexer = Lexer.new testInput
    let tokens = lexAll testLexer
    putStrLn ("tokens -> " ++ show tokens)
    let testParser = Parser.new testLexer []
    let program = parseProgram testParser []
    putStrLn ("length of statements: " ++ show (length program))
    let run = mapM_ print program
    run -}
