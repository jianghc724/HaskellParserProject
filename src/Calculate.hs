{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Calculate where

import Parser

douEval :: Expr -> Double
douEval (Dou p) = p
douEval (Add p q) = (douEval p) + (douEval q)
--eval  = p + q
--eval (Sub p q) = p - q
--eval (Mul p q) = p * q
--eval (Div p q) = p / q
--eval (Eq p q) = (p == q)
--eval (Lt p q) = (p < q)
--eval (Le p q) = (p <= q)
--eval (Gt p q) = (p > q)
--eval (Ge p q) = (p >= q)
