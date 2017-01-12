{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor

data Expr
    = FalseLit
    | TrueLit
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
    deriving Show 

exprParser :: Parser Expr
exprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser
            <|> addParser <|> subParser <|> mulParser <|> divParser <|> eqlParser <|> lesParser <|> leqParser <|> morParser <|> mqlParser

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
    
lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p