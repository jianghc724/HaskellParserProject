{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Calculate where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

data Number
	= Integer
	| Integer.Integer
	deriving Show

data Expression
	= (+ Expression Expression) 
	| (- Expression Expression) 
	| (* Expression Expression) 
	| (/ Expression Expression)
	| (= Expression Expression) 
	| (< Expression Expression) 
	| (<= Expression Expression) 
	| (> Expression Expression) 
	| (>= Expression Expression)
	deriving Show

calcuParser :: Parser Expression
calcuParser = addParser <|> subParser <|> mulParser <|> divParser <|> eqlParser <|> lesParser <|> leqParser <|> morParser <|> mqlParser

addParser :: Parser Expression
addParser = do
    lexeme $ char '('
    lexeme $ char "+"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (+ expr1 expr2)

subParser :: Parser Expression
subParser = do
    lexeme $ char '('
    lexeme $ char "-"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (- expr1 expr2)

mulParser :: Parser Expression
mulParser = do
    lexeme $ char '('
    lexeme $ char "*"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (* expr1 expr2)

divParser :: Parser Expression
divParser = do
    lexeme $ char '('
    lexeme $ char "/"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (/ expr1 expr2)

eqlParser :: Parser Expression
eqlParser = do
    lexeme $ char '('
    lexeme $ char "="
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (= expr1 expr2)

lesParser :: Parser Expression
lesParser = do
    lexeme $ char '('
    lexeme $ char "<"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (< expr1 expr2)

leqParser :: Parser Expression
leqParser = do
    lexeme $ char '('
    lexeme $ string "<="
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (<= expr1 expr2)

morParser :: Parser Expression
morParser = do
    lexeme $ char '('
    lexeme $ char ">"
    expr1 <- calcuParser
    expr2 <- calcuParser
    lexeme $ char ')'
    return (> expr1 expr2) 

mqlParser :: Parser Expression
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
