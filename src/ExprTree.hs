module Lib where

import BoolExpr
import PrettyTreePrint
import REPL
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import System.Environment

data Expr
    = FalseLit
    | TrueLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | + Expr Expr
	| - Expr Expr 
	| * Expr Expr 
	| / Expr Expr
	| = Expr Expr 
	| < Expr Expr 
	| <= Expr Expr 
	| > Expr Expr 
	| >= Expr Expr
    deriving Show 

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p