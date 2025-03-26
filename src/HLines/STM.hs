module HLines.STM where

import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import Control.Concurrent.Async (async, wait)
import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Concurrent (getNumCapabilities)
import Control.Monad (forever, replicateM, forM_)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import HLines.Types
import HLines.Languages
import HLines.Utils

-- | Discover files recursively while respecting ignore patterns.
discoverFiles ::  TQueue FilePath -> FilePath -> [Glob.Pattern] -> IO ()
discoverFiles fileQueue rootPath ignorePatterns = processPath rootPath
  where
    processPath :: FilePath -> IO ()
    processPath path = do
        isFile <- doesFileExist path
        isDir  <- doesDirectoryExist path
        if isFile
          then atomically $ writeTQueue fileQueue path
          else if isDir
            then do
              subIgnorePatterns <- readGitignorePatterns path
              let currentIgnorePatterns = ignorePatterns <> subIgnorePatterns
              entries <- listDirectory path
              let filteredEntries = filter (not . isDefaultIgnoredFolder) entries
              let fullPaths = map (path </>) filteredEntries
              let notIgnored = filter (not . shouldIgnore currentIgnorePatterns) fullPaths
              mapM_ processPath notIgnored
            else return ()

-- | Count lines of code concurrently using STM for sharing the work queue
-- and accumulating stats.
countLinesOfCode :: FilePath -> IO AggratedStats
countLinesOfCode rootPath = do
    numCores <- getNumCapabilities
    -- Build a TQueue of files to process.
    fileQueue <- newTQueueIO

    files <- discoverFiles fileQueue rootPath []

    -- A shared TVar to accumulate statistics.
    statsVar <- newTVarIO mempty

    let worker = do
          -- Try to dequeue a file; if none remain, exit.
          mfile <- atomically $ tryReadTQueue fileQueue
          case mfile of
            Nothing   -> return ()
            Just file -> do
              stats <- processFile file
              -- Update the aggregated stats atomically.
              atomically $ modifyTVar' statsVar (<> stats)
              worker

    -- Spawn a worker per available core.
    workers <- replicateM numCores (async worker)
    mapM_ wait workers

    finalStats <- readTVarIO statsVar
    return $! force finalStats