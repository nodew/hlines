{-# LANGUAGE RecordWildCards #-}

module HLines where

import Control.Parallel.Strategies
import System.Directory
import System.FilePath ((</>))
import System.IO
import Control.Lens ((&), (+~))
import qualified Data.Map.Strict as Map

import HLines.Cmd
import HLines.File
import HLines.Type
import HLines.Directory
import HLines.Language

mergeCount :: Count -> Count -> Count
mergeCount Count{..} count = count & total +~ _total & code +~ _code & blank +~ _blank & comment +~ _comment

mergeByLang :: [(Language, Count)] -> Map.Map Language Count
mergeByLang = Map.fromListWith mergeCount

last2 :: (a, b, c) -> (b, c)
last2 (_, b, c) = (b, c) 

run :: Options -> IO ()
run Options {..} = do
  dir <- getCurrentDirectory
  paths <- mapM listAllFiles files
  let paths' = paths `using` parList rseq
  counts <- mapM countLines $ concat paths
  let counts' = (map last2 counts) `using` parList rseq
  print $ mergeByLang counts'
