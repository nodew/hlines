{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HLines.Parallel where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad (foldM)
import HLines.Languages
import HLines.Types
import HLines.Utils
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob

discoverFiles :: FilePath -> [Glob.Pattern] -> IO [FilePath]
discoverFiles rootPath ignorePatterns = processPath rootPath
  where
    processPath :: FilePath -> IO [FilePath]
    processPath path = do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path

      if isFile
        then return [path]
        else
          if isDir
            then do
              subIgnorePatterns <- readGitignorePatterns path
              let currentIgnorePatterns = ignorePatterns <> subIgnorePatterns
              entries <- listDirectory path
              let filteredEntries = filter (not . isDefaultIgnoredFolder) entries
              let fullPaths = map (path </>) filteredEntries

              let notIgnored = filter (not . shouldIgnore currentIgnorePatterns) fullPaths
              force concat <$> forConcurrently notIgnored processPath
            else return []

countLinesOfCode :: FilePath -> IO AggratedStats
countLinesOfCode rootPath = do
  numCores <- getNumCapabilities
  files <- discoverFiles rootPath []

  let minimunChunkSize = 10
  let chunkSize = max minimunChunkSize (length files `div` numCores)
  let fileChunks = chunksOf chunkSize files

  statsChunks <- forConcurrently fileChunks $ \chunk -> do
    foldM
      ( \acc file -> do
          stats <- processFile file
          let !newAcc = acc <> stats
          return $! force newAcc
      )
      mempty
      chunk

  let !finalStats = foldl (<>) mempty statsChunks

  return $! force finalStats
