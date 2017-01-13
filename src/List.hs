{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module List where

import Parser

eval :: Expr -> [a]
eval NilLit = []
eval Char CharLit = CharLit:[]
eval String StringLit = StringLit
eval Cons Expr1 Expr2
| eval Expr1 == [] || eval Expr2 == [] = (eval Expr1) ++ (eval Expr2)
| type (eval Expr1) == type (eval Expr2) = (eval Expr1) ++ (eval Expr2)
|
eval (Not p) = not $ eval p
eval (And p q) = (eval p) && (eval q) 
eval (Or p q) = (eval p) || (eval q)

-- designed for parseOnly
--   :: Data.Attoparsec.Text.Parser a
--      -> Data.Text.Internal.Text -> Either String a
evalWithErrorThrowing :: Either String Expr -> String
evalWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing (Right expr) = show $ eval expr