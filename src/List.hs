{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module List where

import Parser

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