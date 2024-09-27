module Evaluator where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import qualified Ast
import System.IO
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Lexer.Lexer as Lexer
import Parser

data Fn = Fn {
    parameters :: [Ast.Expression],
    body :: Ast.BlockStatement,
    environment :: Env
}

instance Show Fn where
    show (Fn params body _) =
        "Fn {" ++
        "parameters = " ++ showParams params ++ ", " ++
        "body = " ++ showBody body ++
        "}"
      where
        showParams :: [Ast.Expression] -> String
        showParams ps = intercalate ", " (map show ps)

        showBody :: Ast.BlockStatement -> String
        showBody stmts = "{...}" -- Simplified representation of the function body

instance Show Env where
    show (Env env outer) =
        "Env {\n" ++
        "  env = " ++ showEnvMap env ++ ",\n" ++
        "  outer = " ++ maybe "Nothing" (const "Just <outer environment>") outer ++
        "\n}"
      where
        showEnvMap :: Map.Map String Object -> String
        showEnvMap m =
            "{" ++ intercalate "" (map showKeyValue (Map.toList m)) ++ "}"

        showKeyValue :: (String, Object) -> String
        showKeyValue (k, v) = "    " ++ show k ++ " -> " ++ show v

instance Show BuiltInFunction where
    show (BuiltInFunction name _) = "BuiltInFunction { name = " ++ show name ++ " }"

data BuiltInFunction = BuiltInFunction {
    name :: String,
    builtInFunction :: [Object] -> Object
}

type BuiltIns = Map String BuiltInFunction

data Object =
    Integer Int
    | String String
    | Boolean Bool
    | Null
    | Return Object
    | Assign
    | Function Fn
    | BuiltIn BuiltInFunction
    | Array [Object]
    | Hash (Map Object Object)

instance Show Object where
    show (Integer x) = show x
    show (String x) = x 
    show (Boolean x) = show x
    show Null = "Null"
    show (Return x) = "Return: " ++ show x
    show Assign = "Assign"
    show (Function fn) = "Function with parameters: " ++ showParams (parameters fn)
    show (BuiltIn fn) = "BuiltIn: " ++ name fn  
    show (Array xs) = intercalate "" (map show xs)
    show (Hash m) = "{" ++ intercalate ", " (map showKeyValue (Map.toList m)) ++ "}"

showKeyValue (k, v) = show k ++ " -> " ++ show v

showParams ps = intercalate ", " (map show ps)

instance Ord Object where
    compare (Integer x) (Integer y) = compare x y
    compare (String x) (String y) = compare x y
    compare (Boolean x) (Boolean y) = compare x y
    compare Null Null = EQ
    compare (Integer _) _ = GT
    compare (String _) _ = GT
    compare (Boolean _) _ = GT
    compare _ Null = GT
    compare _ _ = LT

instance Eq Object where
    (Integer x) == (Integer y) = x == y
    (String x) == (String y) = x == y
    (Boolean x) == (Boolean y) = x == y
    (Array x) == (Array y) = x == y
    (Hash x) == (Hash y) = x == y
    Null == Null = True
    _ == _ = False

data Env = Env {
    env :: Map String Object,
    outer :: Maybe Env
}

evalStatements :: Env -> BuiltIns -> [Ast.Statement] -> Object -> (Env, Object)
evalStatements env builtins statements obj =
    if null statements
        then
            (env, obj)
    else
        let hd = head statements in
        let tl = tail statements in
        let (env', obj) = evalStatement env builtins hd in
        evalStatements env' builtins tl obj

evalStatement :: Env -> BuiltIns -> Ast.Statement -> (Env, Object)
evalStatement env builtins statement =
    case statement of
    Ast.LET stmt ->
        let (env', object) = evalExpression env builtins (Ast.value stmt) in
        let env'' = addBinding env' (Ast.name stmt) object in
        (env'', Assign)
    Ast.RETURN stmt -> 
        let (env', object) = evalExpression env builtins stmt in
        (env', Return object)
    Ast.EXPRESSIONSTATEMENT exp -> evalExpression env builtins exp

addBinding :: Env -> String -> Object -> Env
addBinding environment key object =
    case object of
    Function fn ->
        let newMap = Map.insert key (Function $ Fn (parameters fn) (body fn) (Env newMap (outer environment))) (env environment) in
        Env newMap (outer environment) 
    otherwise ->
        let newMap = Map.insert key object (env environment) in
        Env newMap (outer environment)

envGet :: Env -> BuiltIns -> String -> Object
envGet environment builtins value =
  case Map.lookup value (env environment) of
    Just x -> x
    Nothing -> case outer environment of
      Just outerEnv -> envGet outerEnv builtins value
      Nothing -> case Map.lookup value builtins of
        Just x -> BuiltIn x
        Nothing -> error ("unknown identifier: " ++ value)

evalIfExpression :: Env -> BuiltIns -> Ast.IfExpression -> (Env, Object)
evalIfExpression env builtins ifexpression =
    let (env', cond) = evalExpression env builtins condition in
    if isTruthy cond
        then
            evalStatements env' builtins (Ast.body consequence) Null
        else
            case alternative of
            Just x -> evalStatements env' builtins (Ast.body x) Null
            Nothing -> (env', Null)
    where
        condition = Ast.condition ifexpression
        consequence = Ast.consequence ifexpression
        alternative = Ast.alternative ifexpression

        isTruthy obj =
            case obj of
            Boolean x -> x
            Integer x -> x > 0
            {- String x -> null x -}
            Null -> False
            otherwise -> error "invalid truth comparison"

evalInfixExp :: String -> Object -> Object -> Object
evalInfixExp operator left right =
    case (left, right) of
        (Integer x, Integer y) -> evalIntegerInfixExp operator x y
        (Boolean x, Boolean y) -> evalBooleanInfixExp operator x y
        (String x, String y) -> evalStringInfixExp operator x y
    
evalStringInfixExp :: String -> String -> String -> Object
evalStringInfixExp operator left right =
    case operator of
    "PLUS" -> String $ left ++ right
    otherwise -> error "operator does not exist for string"

evalIntegerInfixExp :: String -> Int -> Int -> Object
evalIntegerInfixExp operator left right =
    case operator of
        "MINUS" -> Integer (left - right)
        "PLUS" -> Integer (left + right)
        "ASTERISK" -> Integer (left * right)
        "SLASH" -> Integer $ round (fromIntegral left / fromIntegral right)
        "LT" -> Boolean (left < right)
        "GT" -> Boolean (left > right)
        "EQUAL" -> Boolean (left == right)
        "NOTEQUAL" -> Boolean (left /= right)

evalBooleanInfixExp :: String -> Bool -> Bool -> Object
evalBooleanInfixExp operator left right =
    case operator of
        "EQUAL" -> Boolean (left == right)
        "NOTEQUAL" -> Boolean (left /= right)
        otherwise -> error "expected boolean"

applyFunction :: BuiltIns -> Object -> [Object] -> Object
applyFunction builtins fn args =
    case fn of
    Function fn ->  extendFunctionEnv fn builtins args
    BuiltIn fn -> do builtInFunction fn args
    otherwise -> error "expected function"

getBuiltins :: BuiltIns
getBuiltins =
    Map.fromList $ [
        ("len", BuiltInFunction "len" builtInLen),
        ("head", BuiltInFunction "head" builtInHead),
        ("tail", BuiltInFunction "tail" builtInTail),
        ("push", BuiltInFunction "push" builtInPush),
        ("puts", BuiltInFunction "puts" builtInPuts)
    ]
    where
    builtInLen :: [Object] -> Object
    builtInLen args =
        if length args /= 1
            then
                error ("expected 1 argument to len, received " ++ (show $ length args))
            else
                case head args of
                    String x -> Integer $ length x
                    Array x -> Integer $ length x

    builtInHead :: [Object] -> Object
    builtInHead args =
        if length args /= 1
            then 
                error ("expected 1 argument to head, received" ++ (show $ length args))
        else
            case head args of
                Array x -> if length x == 0 then error "list is empty" else head x
                otherwise -> error "expected array"

    builtInTail :: [Object] -> Object
    builtInTail args =
        if length args /= 1
            then 
                error ("expected 1 argument to tail, received" ++ (show $ length args))
            else
                case head args of
                    Array x ->
                        if length x > 0 
                            then 
                                Array $ tail x 
                            else 
                                Array $ []
                    otherwise -> error "tail expected array"

    builtInPush :: [Object] -> Object
    builtInPush args =
        if length args /= 2
            then 
                error ("expected >2 argument to push, received" ++ (show $ length args))
            else
                case head args of
                    Array x -> Array $ x ++ [args!!1]
                    otherwise -> error ("push expected array, got " ++ show (head args))

    builtInPuts :: [Object] -> Object
    builtInPuts args = String $ intercalate ", " $ map show args

newClosedEnv :: Env -> Env
newClosedEnv inEnv = Env Map.empty $ Just inEnv

setExtendedEnv :: Env -> [Ast.Expression] -> [Object] -> Env
setExtendedEnv environment params args =
    if null params
        then
            environment
        else
            let arg = head args in
            let param = head params in
            case param of
            Ast.IDENTIFIER x ->
                let environment' = Env (Map.insert x arg (env environment)) (outer environment) in
                setExtendedEnv environment' (tail params) (tail args)
            otherwise -> error "expected idents in argument list"

extendFunctionEnv :: Fn -> BuiltIns -> [Object] -> Object
extendFunctionEnv fn builtins args =
    let env = environment fn in
    let params = parameters fn in
    let extendable = newClosedEnv env in
    let extendedEnv = setExtendedEnv extendable params args in
    runFn fn extendedEnv builtins
    where
        runFn fn inEnv builtins = let (env', object) = evalStatements inEnv builtins (Ast.body $ body fn) Null in
            case object of
            Return x -> x
            otherwise -> object

evalExpressions :: Env -> BuiltIns -> [Ast.Expression] -> (Env, [Object])
evalExpressions env builtins expressions =
    foldr step (env, []) expressions
  where
    step exp (currentEnv, results) =
        let (newEnv, result) = evalExpression currentEnv builtins exp
        in (newEnv, result : results)

evalPrefixExpression :: String -> Object -> Object
evalPrefixExpression operator right =
    case operator of
    "BANG" -> evalBangOperatorExpression right
    "MINUS" -> evalMinusOperatorExpression right
    otherwise -> Null

evalBangOperatorExpression :: Object -> Object
evalBangOperatorExpression right =
    case right of
    Integer x -> Boolean $ x <= 0
    Boolean x -> Boolean $ not x
    otherwise -> error "unknown operator for bang"

evalMinusOperatorExpression :: Object -> Object
evalMinusOperatorExpression right =
    case right of
    Integer x -> Integer $ -x
    otherwise -> error "unknown operator for minus"

evalIndexExpression :: Object -> Object -> Object
evalIndexExpression left index =
    case (left, index) of
    (Array x, Integer y) -> evalArrayIndexExpression x y
    (Hash x, _) -> evalHashIndexExpression x (
        case index of
        Integer _ -> index
        Boolean _ -> index
        Null -> error "invalid key: null"
        Return _ -> error "invalid key: return"
        Assign -> error "invalid key: assign"
        Function fn -> index
        BuiltIn fn -> index
        Array _ -> index
        String _ -> index
        Hash _ -> index) 
    otherwise -> error "expected integer index for array"

evalHashIndexExpression :: Map Object Object -> Object -> Object
evalHashIndexExpression hash key =
    case Map.lookup key hash of
        Just x -> x
        Nothing -> error "key not in hash"

evalArrayIndexExpression :: [Object] -> Int -> Object
evalArrayIndexExpression arr index =
    if index < 0 || length arr <= index
        then
            error "index out of bounds"
        else
            arr!!index
    
evalHashLiteral :: Env -> BuiltIns -> Map Ast.Expression Ast.Expression -> Object
evalHashLiteral env builtins inMap =
    let newMap = envHashLiteral' env builtins inMap
    in Hash newMap
  where
    envHashLiteral' env builtins inMap =
        Map.fromList $ map transformPair (Map.toList inMap)

    transformPair :: (Ast.Expression, Ast.Expression) -> (Object, Object)
    transformPair (key, value) =
        let (env', k) = evalExpression env builtins key
            (env'', v) = evalExpression env' builtins value
        in (k, v)

evalExpression :: Env -> BuiltIns -> Ast.Expression -> (Env, Object)
evalExpression env builtins expression =
    case expression of
    Ast.INTEGER int -> (env, Integer int)
    Ast.STRING string -> (env, String string)
    Ast.BOOLEAN bool -> (env, Boolean bool)
    Ast.IDENTIFIER ident -> (env, envGet env builtins ident)
    Ast.PREFIXEXPRESSION prefixexpression ->
        let (env', exp) = evalExpression env builtins (Ast.prefixRight prefixexpression) in
        (env', evalPrefixExpression (Ast.prefixOperator prefixexpression) exp)
    Ast.INFIXEXPRESSION infixexpression ->
        let (env', right) = evalExpression env builtins (Ast.infixRight infixexpression) in
        let (env'', left) = evalExpression env' builtins (Ast.infixLeft infixexpression) in
        (env'', evalInfixExp (Ast.infixOperator infixexpression) left right)
    Ast.IFEXPRESSION ifexpression -> evalIfExpression env builtins ifexpression
    Ast.FUNCTIONLITERAL functionliteral -> 
        (env, Function $ Fn (Ast.parameters functionliteral) (Ast.functionBody functionliteral) env)
    Ast.CALLEXPRESSION callexpression ->
        let arguments = Ast.arguments callexpression in
        let fn = Ast.fn callexpression in
        let (env', evaled) = evalExpression env builtins fn in
        let (env'', args) = evalExpressions env' builtins arguments
        in (env'', applyFunction builtins evaled args)
    Ast.ARRAYLITERAL array ->
        let (env', elems) = evalExpressions env builtins array in
        (env', Array elems)
    Ast.INDEXEXPRESSION indexexpression ->
        let left = Ast.indexLeft indexexpression in
        let index = Ast.index indexexpression in
        let (env', leftObject) = evalExpression env builtins left in
        let (env'', indexObject) = evalExpression env' builtins index in
        (env'', evalIndexExpression leftObject indexObject)
    Ast.HASHLITERAL hashliteral ->
        (env, evalHashLiteral env builtins hashliteral)

repl :: Env -> BuiltIns -> IO ()
repl environment builtins = do
    putStr "type shit >> "
    hFlush stdout
    input <- getLine
    if input == "exit"
        then let _ = putStrLn "Exiting..." in
        exitSuccess
        else do
            let testLexer = Lexer.new input
            let tokens = lexAll testLexer
            {-putStrLn ("tokens -> \n" ++ show tokens)-}
            let testParser = Parser.new testLexer []
            let program = parseProgram testParser []
            {-print "AST -> "
            mapM_ print program-}
            let (environment', eval) = evalStatements environment builtins program Null
            {-print "EVALUATION -> "-}
            print $ show eval
            Evaluator.repl environment' builtins

main = do
    let emptyEnv = Env Map.empty Nothing
    Evaluator.repl emptyEnv getBuiltins
