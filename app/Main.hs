{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import qualified HLines.Conduit as Conduit
import qualified HLines.Parallel as Parallel
import qualified HLines.STM as STM
import qualified HLines.Streamly as Streamly
import HLines.Types (AggregatedStats)
import HLines.Utils
import System.Environment (getArgs)
import System.TimeIt (timeItT)

data Strategy = Parallel | STM | Streamly | Conduit
  deriving (Show, Read)

parseStrategy :: String -> Maybe Strategy
parseStrategy "parallel" = Just Parallel
parseStrategy "stm" = Just STM
parseStrategy "streamly" = Just Streamly
parseStrategy "conduit" = Just Conduit
parseStrategy _ = Nothing

getCountLinesFunc :: Strategy -> (FilePath -> IO AggregatedStats)
getCountLinesFunc Parallel = Parallel.countLinesOfCode
getCountLinesFunc STM = STM.countLinesOfCode
getCountLinesFunc Streamly = Streamly.countLinesOfCode
getCountLinesFunc Conduit = Conduit.countLinesOfCode

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Right (path, strategy) -> runWithStrategy strategy path
    Left err -> do
      putStrLn err
      showUsage

parseArgs :: [String] -> Either String (FilePath, Strategy)
parseArgs [] = Left "No path provided"
parseArgs args = go args Nothing Nothing
  where
    go [] Nothing _ = Left "No path provided"
    go [] (Just path) strategy = Right (path, maybe Conduit id strategy)
    go ("--strategy" : s : rest) path _ = case parseStrategy s of
      Just strat -> go rest path (Just strat)
      Nothing -> Left $ "Invalid strategy: " ++ s
    go ("-s" : s : rest) path _ = case parseStrategy s of
      Just strat -> go rest path (Just strat)
      Nothing -> Left $ "Invalid strategy: " ++ s
    go (arg : rest) Nothing strategy = go rest (Just arg) strategy
    go _ (Just _) _ = Left "Multiple paths provided"

showUsage :: IO ()
showUsage =
  BSC.putStrLn $
    BSC.unlines
      [ "HLines - A Haskell line counting tool for source code",
        "",
        "USAGE:",
        "    hlines <path> [--strategy <strategy>]",
        "",
        "ARGUMENTS:",
        "    <path>    Directory path to analyze",
        "",
        "OPTIONS:",
        "    -s, --strategy <strategy>    Processing strategy to use (default: conduit)",
        "",
        "STRATEGIES:",
        "    parallel    Parallel based processing",
        "    stm         STM based processing",
        "    streamly    Stream processing using Streamly",
        "    conduit     Stream processing using Conduit (default)",
        "",
        "EXAMPLES:",
        "    hlines .",
        "    hlines src --strategy parallel",
        "    hlines /path/to/project -s streamly"
      ]

runWithStrategy :: Strategy -> FilePath -> IO ()
runWithStrategy strategy path = do
  let countLinesFunc = getCountLinesFunc strategy
  (execTime, results) <- timeItT $ countLinesFunc path
  BSC.putStrLn $ formatResults results
  putStrLn $ "Strategy: " ++ show strategy
  putStrLn $ "Execution time: " ++ show execTime
