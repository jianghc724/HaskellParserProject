{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module List where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

data Expr
    = Nil
    | Cons Expr Expr
    | Car Expr
    | Cdr Expr
    | CharLit
    | StringLit
    deriving Show

nilListParser :: Parser Expr
nilListParser = lexeme $ string "()" $> Nil

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
    expr1 <- exprParser
    lexeme $ string "cdr"
    expr2 <- exprParser
    lexeme $ char ')'
    return (Cdr expr)

eval :: Expr -> Bool
eval FalseLit = False
eval TrueLit = True
eval (Not p) = not $ eval p
eval (And p q) = (eval p) && (eval q) 
eval (Or p q) = (eval p) || (eval q)

-- designed for parseOnly
--   :: Data.Attoparsec.Text.Parser a
--      -> Data.Text.Internal.Text -> Either String a
evalWithErrorThrowing :: Either String Expr -> String
evalWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing (Right expr) = show $ eval expr