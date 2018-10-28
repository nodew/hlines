{-# LANGUAGE RecordWildCards #-}

module HLines where

import Control.Lens ((&), (+~))
import qualified Data.Map.Strict as Map
-- import Control.Parallel.Strategies
import System.Directory
import System.FilePath ((</>), takeExtension)
import System.IO

import Conduit
import Data.Conduit.Combinators (yieldMany)
import System.FilePath.GlobPattern

import HLines.Cmd
import HLines.Directory
import HLines.File
import HLines.Language
import HLines.Type

mergeCount :: Count -> Count -> Count
mergeCount Count {..} count = count & total +~ _total & code +~ _code & blank +~ _blank & comment +~ _comment

mergeByLang :: Map.Map Language Count -> (Language, Count) -> Map.Map Language Count
mergeByLang m (lang, count) =
  case Map.lookup lang m of
    Nothing -> Map.insert lang count m
    Just p -> Map.adjust (mergeCount count) lang m

runProg :: Options -> IO ()
runProg Options {..} = do
  result <- runConduitRes
          $ yieldMany files
         .| sourceDir include exclude
         .| filterC (\fp ->
                       case match of
                         Nothing -> True
                         Just match' -> all (\p -> fp ~~ p) match')
         .| mapMC (\f -> do
                      (lang, content) <- liftIO $ readContent f
                      return (f, lang, content))
         .| mapC (\(f, lang, content) -> (lang, countLines lang content))
         .| foldlC mergeByLang (Map.empty :: Map.Map Language Count)
  print result
