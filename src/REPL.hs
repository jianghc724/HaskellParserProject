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

--procPro :: Env -> Program -> Either Env ExprVal
--procPro env (Pro p) = Left (procStat env p)
--procPro env (Cal p) = Right (eval p env)

--getProEnv :: Either Env ExprVal -> 

mainLoop :: Env -> String -> IO ()
mainLoop env lastSentence = do
    putStr "> "
    hFlush stdout
    l <- getLine
    case Prelude.words l of
        ":i":pro -> do 
            putStrLn $ show $ procPro env (getPro (parseOnly allParser (pack (getWord pro)))) 
            --putStrLn $ show $ (procPro env) (parseOnly allParser (pack (getWord pro)))
            --mainLoop (either (errorhandle env) (procPro env) (parseOnly allParser (pack (getWord pro)))) pro
        [":t"] -> putStrLn "To do"
        [":q"] -> putStrLn "Bye~"
        _ -> do
            putStrLn "unrecognized command!"
            mainLoop env lastSentence
            
--defMain :: IO ()
--defMain = do
--    putStrLn "This is a simple REPL. Be my guest!"
--    mainLoop (M.empty)