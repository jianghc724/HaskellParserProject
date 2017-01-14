{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

data Expr
    = FalseLit
    | TrueLit
    | VarLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Add Expr Expr
    | Sub Expr Expr 
    | Mul Expr Expr 
    | Div Expr Expr
    | Eq Expr Expr 
    | Lt Expr Expr 
    | Le Expr Expr 
    | Gt Expr Expr 
    | Ge Expr Expr
    | NilLit
    | Cons Expr Expr
    | Car Expr
    | Cdr Expr
    | Chr Char
    | St String
    | Int Integer
    | Dou Double
    deriving Show

data Statement
    = Begin Statement Statements
    | Skip
    | Set Expr Expr
    | If Expr Statement Statement
    | While Expr Statement
    deriving Show

data Statements
    = Nil
    | List Statement Statements
    deriving Show
    
data Program
    = Pro Statement
    deriving Show

--data Number = Integer
--    | Double
--    deriving Show

exprParser :: Parser Expr

exprParser = nilListParser <|> charParser <|> stringParser <|> consParser <|> carParser <|> cdrParser<|> falseParser <|> trueParser <|> notParser <|> andParser <|> orParser
            <|> addParser <|> subParser <|> mulParser <|> divParser <|> eqlParser 
            <|> lesParser <|> leqParser <|> morParser <|> mqlParser <|> douParser <|> intParser
            <|> variableParser

intParser :: Parser Expr
intParser = do 
    ds <- many1 digit
    return (Int (read ds))

douParser :: Parser Expr
douParser = do 
    d <- lexeme $ double 
    return (Dou d) 

            
statParser :: Parser Statement
statParser = setParser <|> skipParser <|> ifParser <|> whilestatParser <|> statlistParser


statsParser :: Parser Statements
statsParser = statslistParser <|> nilParser

whileParser :: Parser Program
whileParser = do
    stat <- statParser
    return (Pro stat)
            
variableParser :: Parser Expr
variableParser = do
    lexeme $ string "a" $> VarLit
            
falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ char ')'
    return (Not expr)
    
andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (And expr1 expr2)
    
orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Or expr1 expr2)
    
addParser :: Parser Expr
addParser = do
    lexeme $ char '('
    lexeme $ char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ char '('
    lexeme $ char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Sub expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ char '('
    lexeme $ char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Mul expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ char '('
    lexeme $ char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Div expr1 expr2)

eqlParser :: Parser Expr
eqlParser = do
    lexeme $ char '('
    lexeme $ char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Eq expr1 expr2)

lesParser :: Parser Expr
lesParser = do
    lexeme $ char '('
    lexeme $ char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Lt expr1 expr2)

leqParser :: Parser Expr
leqParser = do
    lexeme $ char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Le expr1 expr2)

morParser :: Parser Expr
morParser = do
    lexeme $ char '('
    lexeme $ char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Gt expr1 expr2) 

mqlParser :: Parser Expr
mqlParser = do
    lexeme $ char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Ge expr1 expr2)

nilListParser :: Parser Expr
nilListParser = lexeme $ string "nil" $> NilLit

consParser :: Parser Expr
consParser = do
    lexeme $ char '('
    lexeme $ string "cons"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
    lexeme $ char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ char ')'
    return (Car expr)
    
cdrParser :: Parser Expr
cdrParser = do
    lexeme $ char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ char ')'
    return (Cdr expr)

charParser :: Parser Expr
charParser = do
    lexeme $ char '\'' 
    c <- anyChar
    lexeme $ char '\''
    return (Chr c)

stringParser :: Parser Expr
stringParser = do
    lexeme $ char '\"'
    s <- takeWhile1 (\x -> if x == '\"' then True else False)
    lexeme $ char '\"'
    return (St s)
    
setParser :: Parser Statement
setParser = do
    lexeme $ char '('
    lexeme $ string "set!"
    var <- variableParser
    expr <- exprParser
    lexeme $ char ')'
    return (Set var expr)
    
skipParser :: Parser Statement
skipParser = do
    lexeme $ string "skip"
    return Skip
    
ifParser :: Parser Statement
ifParser = do
    lexeme $ char '('
    lexeme $ string "if"
    expr <- exprParser
    stat1 <- statParser
    stat2 <- statParser
    lexeme $ char ')'
    return (If expr stat1 stat2)
    
whilestatParser :: Parser Statement
whilestatParser = do
    lexeme $ char '('
    lexeme $ string "while"
    expr <- exprParser
    stat <- statParser
    lexeme $ char ')'
    return (While expr stat)
    
statlistParser :: Parser Statement
statlistParser = do
    lexeme $ char '('
    lexeme $ string "begin"
    stat <- statParser
    stats <- statsParser
    return (Begin stat stats)
    
statslistParser :: Parser Statements
statslistParser = do
    stat <- statParser
    stats <- statsParser
    return (List stat stats)
    
nilParser :: Parser Statements
nilParser = do
    lexeme $ char ')'
    return Nil
 
lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

data ExprVal = Num | Bool | Char | ]String | [ExprVal]
    deriving show
instance Eq ExprVal where
    Num == Num = True
    Bool == Bool = True
    Char == Char = True
    [ExprVal] == [ExprVal] = True
    [Char] == String = True
    _ ==_ = False

eval :: Expr -> ExprVal
eval FalseLit = False
eval TrueLit = True
eval (Not p) = not $ eval p
eval (And p q) = (eval p) && (eval q) 
eval (Or p q) = (eval p) || (eval q)
eval (Eq p q) = (douEval p) == (douEval q)
eval (Lt p q) = (douEval p) < (douEval q)
eval (Le p q) = (douEval p) <= (douEval q)
eval (Gt p q) = (douEval p) > (douEval q)
eval (Ge p q) = (douEval p) >= (douEval q)

eval (Dou p) = p
eval (Add p q) = (douEval p) + (douEval q)
eval (Sub p q) = (douEval p) - (douEval q)
eval (Mul p q) = (douEval p) * (douEval q)
eval (Div p q) = (douEval p) / (douEval q)

eval NilLit = []
eval Chr c = c
eval St s = s
eval Cons e1 e2
    | e2 == [] = e1:e2
    | e2 == x:xs && (type e1) == (type x) = e1:e2
    | otherwise (error "temp error")
eval Car (Cons e1 e2) = eval e1
eval Cdr (Cons e1 e2) = eval e2

eval (Not p) = not $ eval p
eval (And p q) = (eval p) && (eval q) 
eval (Or p q) = (eval p) || (eval q)

getExpr :: Either String Expr -> String
getExpr (Left errStr) =  "not a valid expr: " ++ errStr
getExpr (Right expr) = show $ eval expr

defMain :: IO ()
defMain = do
    putStrLn $ show $ parseOnly notParser "(not True)"
    putStrLn $ getExpr $ parseOnly addParser "(+ 1.2 2.2 )" 
    putStrLn $ getExpr $ parseOnly mulParser "(* 2 2.2 )" 
    putStrLn $ getExpr $ parseOnly divParser "(/ 10 2 )" 
    putStrLn $ show $ parseOnly exprParser "12.3"
    putStrLn $ getExpr $ parseOnly charParser "\'a\'" 
    putStrLn $ getExpr $ parseOnly stringParser "\"abc\""
    putStrLn $ getExpr $ parseOnly consParser "(cons \'a\' \'b\')"
    putStrLn "-------"





