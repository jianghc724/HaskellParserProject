{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module Calculate where

import Parser

data Number
	= Integer
	| Integer.Integer
	deriving Show
