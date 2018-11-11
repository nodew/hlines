module Main where

import System.Environment
import System.IO
import System.Directory
import System.Console.CmdArgs

import HLines.Pipes
import HLines.Cmd

main :: IO ()
main = do
  opts <- cmdArgsRun mode
  runP opts
