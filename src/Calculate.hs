{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Calculate where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import ExprTree

data Number
	= Integer
	| Integer.Integer
	deriving Show

calcuParser :: Parser Expr
calcuParser = addParser <|> subParser <|> mulParser <|> divParser <|> eqlParser <|> lesParser <|> leqParser <|> morParser <|> mqlParser

addParser :: Parser Expr
addParser = do
    lexeme $ char '('
    lexeme $ char "+"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (+ expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ char '('
    lexeme $ char "-"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (- expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ char '('
    lexeme $ char "*"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (* expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ char '('
    lexeme $ char "/"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (/ expr1 expr2)

eqlParser :: Parser Expr
eqlParser = do
    lexeme $ char '('
    lexeme $ char "="
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (= expr1 expr2)

lesParser :: Parser Expr
lesParser = do
    lexeme $ char '('
    lexeme $ char "<"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (< expr1 expr2)

leqParser :: Parser Expr
leqParser = do
    lexeme $ char '('
    lexeme $ string "<="
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (<= expr1 expr2)

morParser :: Parser Expr
morParser = do
    lexeme $ char '('
    lexeme $ char ">"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (> expr1 expr2) 

mqlParser :: Parser Expr
mqlParser = do
    lexeme $ char '('
    lexeme $ string ">="
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (>= expr1 expr2)

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p
