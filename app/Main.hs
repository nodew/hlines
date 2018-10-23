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
  run opts
  -- dir <- getCurrentDirectory
  -- let entrypoint = args !! 0
  --     fullpath = compeletePath dir entrypoint
  -- files <- dirWalkDefault fullpath
  -- counts <- mapM (\file -> countLines file) files
  -- putStrLn $ show $ sum counts
