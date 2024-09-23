module Ast where
import Distribution.FieldGrammar (Set')

data Statement =
    LET LetStatement
    | RETURN Expression
    | EXPRESSIONSTATEMENT Expression
    deriving Show

newtype BlockStatement = BlockStatement {
    body :: [Statement]
} deriving Show

data LetStatement = LetStatement {
    name :: String,
    value :: Expression
} deriving Show

data PrefixExpression = PrefixExpression {
    prefixOperator :: String,
    prefixRight :: Expression
} deriving Show

data InfixExpression = InfixExpression {
    infixLeft :: Expression,
    infixOperator :: String,
    infixRight :: Expression
} deriving Show

data IfExpression = IfExpression {
    condition :: Expression,
    consequence :: BlockStatement,
    alternative :: Maybe BlockStatement
} deriving Show

data FunctionLiteral = FunctionLiteral {
    parameters :: [Expression],
    functionBody :: BlockStatement
} deriving Show

data CallExpression = CallExpression {
    fn :: Expression,
    arguments :: [Expression]
} deriving Show

data Expression =
    IDENTIFIER String
    | INTEGER Int
    | PREFIXEXPRESSION PrefixExpression
    | INFIXEXPRESSION InfixExpression
    | BOOLEAN Bool
    | IFEXPRESSION IfExpression
    | FUNCTIONLITERAL FunctionLiteral
    | CALLEXPRESSION CallExpression
    deriving Show

newtype Program = Program {
    statements :: [Statement]
} deriving Show