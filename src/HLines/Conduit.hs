{-# LANGUAGE BangPatterns #-}

module HLines.Conduit where

import Conduit hiding (fst)
import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.Binary as CB
import HLines.Languages
import HLines.Types
import HLines.Utils hiding (processFile)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import qualified System.FilePath.Glob as Glob
import System.IO (IOMode (..), hSetEncoding, utf8, withFile)

countFileLinesConduit :: FilePath -> Language -> IO FileStats
countFileLinesConduit file lang = do
  exists <- doesFileExist file
  if not exists
    then return mempty
    else do
      (stats, _) <-
        runConduitRes $
          CB.sourceFile file
            .| CB.lines
            .| foldlC (processLine lang) (mempty, [])
      return $! force stats

processFile :: FilePath -> IO AggregatedStats
processFile file = case identifyLanguage file of
  Nothing -> return mempty
  Just lang -> do
    stats <- countFileLinesConduit file lang
    return $! singleAggregatedStats (name lang) stats

getFilePathsConduit :: FilePath -> ConduitT () FilePath (ResourceT IO) ()
getFilePathsConduit root = yield root .| traverseDir []
  where
    traverseDir patterns = awaitForever $ \dir -> do
      isDir <- liftIO $ doesDirectoryExist dir
      if isDir
        then do
          subIgnorePatterns <- liftIO $ readGitignorePatterns root
          let currentIgnorePatterns = patterns <> subIgnorePatterns
          entries <- liftIO $ listDirectory dir
          let filteredEntries = filter (not . isDefaultIgnoredFolder) entries
          let paths = map (dir </>) filteredEntries
              filtered = filter (\p -> not $ any (\pat -> Glob.match pat p) currentIgnorePatterns) paths
          yieldMany filtered .| traverseDir patterns
        else yield dir

countLinesOfCode :: FilePath -> IO AggregatedStats
countLinesOfCode root = do
  numCores <- getNumCapabilities
  runConduitRes $
    getFilePathsConduit root
      .| mapMC (liftIO . processFile)
      .| foldlC (<>) mempty
