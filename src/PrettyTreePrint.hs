{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PrettyTreePrint where

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

data Tree a = Nil | Cons a String (Tree a) (Tree a) (Tree a)
    deriving (Show, Generic, Out)
    
genTree :: Int -> Tree Int
genTree n
    | n < 0 = error "expect non-negative argument"
    | n == 0 = Nil
    | otherwise = Cons n "s" t t t where
        t = genTree (n - 1)
    
printDoc :: IO ()
printDoc = putStrLn (render (doc (genTree 5)))

-- defMain = printDoc