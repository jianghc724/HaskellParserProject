{-# LANGUAGE OverloadedStrings #-}
-- 若在 ghci 中执行，需 :set -XOverloadedStrings

module FileIO where

import System.IO
import Data.Char(toUpper)

processFile :: String -> String -> IO()
processFile inFile outFile = do
    inh <- openFile inFile ReadMode
    ouh <- openFile outFile WriteMode
    processLine inh ouh
    hClose inh
    hClose ouh

processLine :: Handle -> Handle -> IO()
processLine inh ouh = 
	do isEof <- hIsEOF inh
	   if isEof 
	        then return()
	        else do lineStr <- hGetLine inh
	    	        hPutStrLn ouh (map toUpper lineStr)
	    	        processLine inh ouh