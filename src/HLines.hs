{-# LANGUAGE RecordWildCards #-}

module HLines where

import Control.Parallel.Strategies
import System.Directory
import System.FilePath ((</>))
import System.IO

import HLines.Cmd
import HLines.File
import HLines.Type

run :: Options -> IO ()
run Options {..} = do
  dir <- getCurrentDirectory
  paths <- mapM dirWalkDefault files  
  counts <- mapM countLines $ concat paths
  let counts' = counts `using` parList rseq
  mapM_ print counts'
