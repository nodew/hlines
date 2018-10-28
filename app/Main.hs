module Main where

import System.Environment
import System.IO
import System.Directory
import System.Console.CmdArgs

import HLines
import HLines.Cmd

main :: IO ()
main = do
  opts <- cmdArgsRun mode
  runProg opts
