{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLines.Streamly where

import Control.Concurrent (getNumCapabilities)
-- Streamly imports

import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import HLines.Languages
import HLines.Types
import HLines.Utils
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Prelude as SP
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import qualified System.FilePath.Glob as Glob
import System.IO (Handle, IOMode (..), hIsEOF, hSetEncoding, utf8, withFile)

-- Stream files from the filesystem
discoverFiles :: FilePath -> [Glob.Pattern] -> Stream IO FilePath
discoverFiles rootPath ignorePatterns = processPath rootPath
  where
    processPath :: FilePath -> Stream IO FilePath
    processPath path =
      Stream.handle (\(_ :: SomeException) -> pure $ Stream.fromList []) $
        Stream.concatMapM processDirectory $
          Stream.fromList [path]

    processDirectory :: FilePath -> IO (Stream IO FilePath)
    processDirectory p = handleIOErrors p $ do
      isFile <- doesFileExist p
      isDir <- doesDirectoryExist p

      if isFile
        then pure $ Stream.fromList [p]
        else
          if isDir
            then do
              subIgnorePatterns <- readGitignorePatterns p
              let currentIgnorePatterns = ignorePatterns <> subIgnorePatterns
              entries <- listDirectory p
              let filteredEntries = filter (not . isDefaultIgnoredFolder) entries
              let fullPaths = map (p </>) filteredEntries

              let notIgnored = filter (not . shouldIgnore currentIgnorePatterns) fullPaths
              files <- filterM doesFileExist notIgnored
              dirs <- filterM doesDirectoryExist notIgnored

              -- Process files and directories sequentially
              pure $
                Stream.fromList files
                  `Stream.append` Stream.concatMap processPath (Stream.fromList dirs)
            else pure $ Stream.fromList []

    handleIOErrors :: FilePath -> IO a -> IO a
    handleIOErrors p action =
      try action >>= \case
        Left (e :: SomeException) -> pure $ error $ "Error processing " ++ p ++ ": " ++ show e
        Right result -> pure result

-- Process files concurrently and aggregate stats
countLinesOfCode :: FilePath -> IO AggregatedStats
countLinesOfCode rootPath = do
  numCores <- getNumCapabilities
  -- Process files with basic concurrency
  Stream.fold foldStats $
    SP.parMapM (SP.maxThreads numCores) processFile $
      Stream.filter (isJust . identifyLanguage) $
        discoverFiles rootPath []
  where
    foldStats :: Fold IO AggregatedStats AggregatedStats
    foldStats = Fold.foldl' (<>) mempty
