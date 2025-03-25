{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.TimeIt (timeItT)
import qualified Data.Text.IO as TIO

import HLines.Conduit
import HLines.Utils

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            (execTime, results) <- timeItT $ countLinesOfCode path
            TIO.putStrLn $ formatResults results
            putStrLn $ "Execution time: " ++ show execTime
        _ -> TIO.putStrLn "Usage: hlines <path>"
