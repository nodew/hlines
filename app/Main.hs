{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.TimeIt (timeItT)
import qualified Data.ByteString.Char8 as BSC

import HLines.STM
import HLines.Utils

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            (execTime, results) <- timeItT $ countLinesOfCode path
            BSC.putStrLn $ formatResults results
            putStrLn $ "Execution time: " ++ show execTime
        _ -> BSC.putStrLn "Usage: hlines <path>"
