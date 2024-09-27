module Ast where

import Data.Map (Map)
import qualified Data.Map as Map

data Statement =
    LET LetStatement
    | RETURN Expression
    | EXPRESSIONSTATEMENT Expression
    deriving (Show, Eq, Ord)

newtype BlockStatement = BlockStatement {
    body :: [Statement]
} deriving (Show, Eq, Ord)

data LetStatement = LetStatement {
    name :: String,
    value :: Expression
} deriving (Show, Eq, Ord)

data PrefixExpression = PrefixExpression {
    prefixOperator :: String,
    prefixRight :: Expression
} deriving (Show, Eq, Ord)

data InfixExpression = InfixExpression {
    infixLeft :: Expression,
    infixOperator :: String,
    infixRight :: Expression
} deriving (Show, Eq, Ord)

data IfExpression = IfExpression {
    condition :: Expression,
    consequence :: BlockStatement,
    alternative :: Maybe BlockStatement
} deriving (Show, Eq, Ord)

data FunctionLiteral = FunctionLiteral {
    parameters :: [Expression],
    functionBody :: BlockStatement
} deriving (Show, Eq, Ord)

data CallExpression = CallExpression {
    fn :: Expression,
    arguments :: [Expression]
} deriving (Show, Eq, Ord)

data IndexExpression = IndexExpression {
    indexLeft :: Expression,
    index :: Expression
} deriving (Show, Eq, Ord)

data Expression =
    IDENTIFIER String
    | INTEGER Int
    | STRING String
    | PREFIXEXPRESSION PrefixExpression
    | INFIXEXPRESSION InfixExpression
    | BOOLEAN Bool
    | IFEXPRESSION IfExpression
    | FUNCTIONLITERAL FunctionLiteral
    | CALLEXPRESSION CallExpression
    | ARRAYLITERAL [Expression]
    | INDEXEXPRESSION IndexExpression
    | HASHLITERAL (Map Expression Expression) 
    deriving (Show, Eq, Ord)

newtype Program = Program {
    statements :: [Statement]
} deriving (Show, Eq, Ord)
