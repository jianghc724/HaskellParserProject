{-# LANGUAGE OverloadedStrings #-}

module REPL where

import qualified Data.Map as M
import System.IO
import Data.Attoparsec.Text
import Data.Functor
import Data.Text.Internal
import Data.Text
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty
import Parser

errorhandle :: Env -> String -> Env
errorhandle env str = env


getWord :: [[Char]] -> [Char]
getWord x
    | Prelude.length x == 1 = Prelude.head x
    | otherwise = Prelude.head x ++ " " ++ getWord (Prelude.tail x) 

mainLoop :: Env -> IO ()
mainLoop env = do
    putStr "> "
    hFlush stdout
    l <- getLine
    case Prelude.words l of
        ":i":pro -> do 
            putStrLn "loop"
            --mainLoop (either (errorhandle env) (procPro env) (parseOnly allParser (pack (getWord pro))))
        [":t"] -> putStrLn "To do"
        [":q"] -> putStrLn "Bye~"
        _ -> do
            putStrLn "unrecognized command!"
            mainLoop env
            
--defMain :: IO ()
--defMain = do
--    putStrLn "This is a simple REPL. Be my guest!"
--    mainLoop (M.empty)