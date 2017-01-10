{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Array where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

data ArrayExpr
    = Nil
    | Cons ArrayExpr ArrayExpr
    | Car ArrayExpr
    | Cdr ArrayExpr
    | CharLit
    | StringLit
    deriving Show
    
exprParser :: Parser Expr
exprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser

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
    expr1 <- exprParser
    lexeme $ string "and"
    expr2 <- exprParser
    lexeme $ char ')'
    return (And expr1 expr2)
    
orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    expr1 <- exprParser
    lexeme $ string "or"
    expr2 <- exprParser
    lexeme $ char ')'
    return (Or expr1 expr2)
