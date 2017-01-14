{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M

type Env = M.Map ExprVal ExprVal

data Tree a = Nil | Node a (Tree a) (Tree a) (Tree a)
    deriving (Show, Generic, Out)

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
    | Ch Char
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
    = NilStat
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
    d <- lexeme $ Data.Attoparsec.Text.double 
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
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Not expr)
    
andParser :: Parser Expr
andParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (And expr1 expr2)
    
orParser :: Parser Expr
orParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Or expr1 expr2)
    
addParser :: Parser Expr
addParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Sub expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Mul expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Div expr1 expr2)

eqlParser :: Parser Expr
eqlParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Eq expr1 expr2)

lesParser :: Parser Expr
lesParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Lt expr1 expr2)

leqParser :: Parser Expr
leqParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Le expr1 expr2)

morParser :: Parser Expr
morParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ Data.Attoparsec.Text.char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Gt expr1 expr2) 

mqlParser :: Parser Expr
mqlParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Ge expr1 expr2)

nilListParser :: Parser Expr
nilListParser = lexeme $ string "nil" $> NilLit

consParser :: Parser Expr
consParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "cons"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "car"
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Car expr)
    
cdrParser :: Parser Expr
cdrParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "cdr"
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Cdr expr)

charParser :: Parser Expr
charParser = do
    lexeme $ Data.Attoparsec.Text.char '\'' 
    c <- anyChar
    lexeme $ Data.Attoparsec.Text.char '\''
    return (Ch c)

stringParser :: Parser Expr
stringParser = do
    lexeme $ Data.Attoparsec.Text.char '\"'
    s <- takeWhile1 (\x -> if x == '\"' then True else False)
    lexeme $ Data.Attoparsec.Text.char '\"'
    return (St (showText s))
    
setParser :: Parser Statement
setParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "set!"
    var <- variableParser
    expr <- exprParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (Set var expr)
    
skipParser :: Parser Statement
skipParser = do
    lexeme $ string "skip"
    return Skip
    
ifParser :: Parser Statement
ifParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "if"
    expr <- exprParser
    stat1 <- statParser
    stat2 <- statParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (If expr stat1 stat2)
    
whilestatParser :: Parser Statement
whilestatParser = do
    lexeme $ Data.Attoparsec.Text.char '('
    lexeme $ string "while"
    expr <- exprParser
    stat <- statParser
    lexeme $ Data.Attoparsec.Text.char ')'
    return (While expr stat)
    
statlistParser :: Parser Statement
statlistParser = do
    lexeme $ Data.Attoparsec.Text.char '('
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
    lexeme $ Data.Attoparsec.Text.char ')'
    return NilStat
 
lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

data ExprVal = ExprDou Double | ExprBool Bool | ExprChar Char | ExprString String | ExprList [ExprVal] | ExprCons (ExprVal, ExprVal)
instance Show ExprVal where
    show (ExprDou num) = show num
    show (ExprBool bool) = show bool
    show (ExprChar char) = show char
    show (ExprString string) = show string
    show (ExprList list) = show list
    show (ExprCons pair) = show pair

instance Eq ExprVal where
    (==) (ExprDou num1) (ExprDou num2) = num1 == num2
    (==) (ExprBool bool1) (ExprBool bool2) = bool1 == bool2
    (==) (ExprChar char1) (ExprChar char2) = char1 == char2
    (==) (ExprString string1) (ExprString string2) = string1 == string2
    (==) (ExprList list1) (ExprList list2)  = list1 == list2
    (==) (ExprList pair1) (ExprList pair2)  = pair1 == pair2

instance Ord ExprVal where
    (>=) (ExprDou num1) (ExprDou num2) = num1 >= num2
    (>) (ExprDou num1) (ExprDou num2) = num1 > num2
    (<=) (ExprDou num1) (ExprDou num2) = num1 <= num2
    (<) (ExprDou num1) (ExprDou num2) = num1 < num2

evalDou :: ExprVal -> Double
evalDou (ExprDou num) = num
evalBool :: ExprVal -> Bool
evalBool (ExprBool bool) = bool
evalChar :: ExprVal -> Char
evalChar (ExprChar char) = char
evalString :: ExprVal -> String
evalString (ExprString string) = string
evalList :: ExprVal -> [ExprVal]
evalList (ExprList list) = list
evalCons :: ExprVal -> (ExprVal, ExprVal)
evalCons (ExprCons pair) = pair

eval :: Expr -> ExprVal
eval FalseLit = ExprBool False
eval TrueLit = ExprBool True
eval (Not p) = ExprBool (not (evalBool(eval p)))
eval (And p q) = ExprBool ((evalBool (eval p)) && (evalBool (eval q)))
eval (Or p q) = ExprBool ((evalBool (eval p)) || (evalBool (eval q)))
eval (Eq p q) = ExprBool ((eval p) == (eval q))
eval (Lt p q) = ExprBool ((evalDou (eval p)) < (evalDou (eval q)))
eval (Le p q) = ExprBool ((evalDou (eval p)) <= (evalDou (eval q)))
eval (Gt p q) = ExprBool ((evalDou (eval p)) > (evalDou (eval q)))
eval (Ge p q) = ExprBool ((evalDou (eval p)) >= (evalDou (eval q)))
eval (Dou p) = ExprDou p
eval (Add p q) = ExprDou ((evalDou (eval p)) + (evalDou (eval q)))
eval (Sub p q) = ExprDou ((evalDou (eval p)) - (evalDou (eval q)))
eval (Mul p q) = ExprDou ((evalDou (eval p)) * (evalDou (eval q)))
eval (Div p q) = ExprDou ((evalDou (eval p)) / (evalDou (eval q)))

eval NilLit = ExprCons ()::(ExprVal, ExprVal)
eval Ch c = ExprChar c
eval St s = ExprString s
eval Cons (Dou d) NilLit = eval (Dou d)
eval Cons FalseLit NilLit = eval FalseLit
eval Cons TrueLit NilLit = eval TrueLit
eval Cons e1 e2 = ExprCons (eval e1, eval e2)
eval Car NilLit = ExprCons ()::(ExprVal, ExprVal)
eval Car Cons e1 e2 = eval e1
eval Car NilLit = ExprCons ()::(ExprVal, ExprVal)
eval Cdr Cons e1 e2 = eval e2

getExpr :: Either String Expr -> String
getExpr (Left errStr) =  "not a valid expr: " ++ errStr
getExpr (Right expr) = show $ eval expr

genExprTree :: Expr -> Tree String
genExprTree FalseLit = Node "False" Nil Nil Nil
genExprTree TrueLit = Node "True" Nil Nil Nil
genExprTree (Not p) = Node "not" (genExprTree p) Nil Nil
genExprTree (And p q) = Node "and" (genExprTree p) (genExprTree q) Nil
genExprTree (Or p q) = Node "or" (genExprTree p) (genExprTree q) Nil
genExprTree (Add p q) = Node "+" (genExprTree p) (genExprTree q) Nil
genExprTree (Sub p q) = Node "-" (genExprTree p) (genExprTree q) Nil
genExprTree (Mul p q) = Node "*" (genExprTree p) (genExprTree q) Nil
genExprTree (Div p q) = Node "/" (genExprTree p) (genExprTree q) Nil
genExprTree (Eq p q) = Node "==" (genExprTree p) (genExprTree q) Nil
genExprTree (Lt p q) = Node "<" (genExprTree p) (genExprTree q) Nil
genExprTree (Le p q) = Node "<=" (genExprTree p) (genExprTree q) Nil
genExprTree (Gt p q) = Node ">" (genExprTree p) (genExprTree q) Nil
genExprTree (Ge p q) = Node ">=" (genExprTree p) (genExprTree q) Nil
genExprTree (Int p) = Node (show p) Nil Nil Nil
genExprTree (Dou p) = Node (show p) Nil Nil Nil

genStatTree :: Statement -> Tree String
genStatTree (Begin p q) = Node "begin" (genStatTree p) (genStatsTree q) Nil
genStatTree Skip = Node "skip" Nil Nil Nil
genStatTree (Set p q) = Node "set" (genExprTree p) (genExprTree q) Nil
genStatTree (If p q r) = Node "if" (genExprTree p) (genStatTree q) (genStatTree r)
genStatTree (While p q) = Node "while" (genExprTree p) (genStatTree q) Nil

genStatsTree :: Statements -> Tree String
genStatsTree NilStat = Node "nil" Nil Nil Nil
genStatsTree (List p q) = Node "statement_list" (genStatTree p) (genStatsTree q) Nil

genProTree :: Program -> Tree String
genProTree (Pro p) = Node "program" (genStatTree p) Nil Nil

procStat :: Env -> Statement -> Env
procStat env (Begin p q) = (procStats (procStat env p) q)
procStat env (Skip) = env
procStat env (Set var p) = (M.insert (eval var) (eval p) env)
procStat env (If expr p q) = if evalBool (eval expr) then (procStat env p) else (procStat env q)
procStat env (While expr p) = if evalBool (eval expr) then (procStat env p) else env
    
procStats :: Env -> Statements -> Env
procStats env (NilStat) = env
procStats env (List p q) = (procStats (procStat env p) q)

defMain :: IO ()
defMain = do
    putStrLn $ getExpr $ parseOnly notParser "(not True)"
    putStrLn $ getExpr $ parseOnly addParser "(+ 1.2 2.2 )" 
    putStrLn $ getExpr $ parseOnly mulParser "(* 2 2.2 )" 
    putStrLn $ getExpr $ parseOnly divParser "(/ 10 2 )" 
    putStrLn $ show $ parseOnly exprParser "12.3"
    putStrLn $ getExpr $ parseOnly charParser "\'a\'" 
    putStrLn $ getExpr $ parseOnly stringParser "\"abc\""
    putStrLn $ getExpr $ parseOnly consParser "(cons \'a\' \'b\')"
    putStrLn "-------"
