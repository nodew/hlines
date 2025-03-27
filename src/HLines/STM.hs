module HLines.STM where

import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, replicateM, when)
import GHC.IO (throwIO)
import HLines.Languages
import HLines.Types
import HLines.Utils
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob

type FileTQueue = TQueue FilePath

data TraversalState = TraversalState
  { activeDirs :: TVar Int,
    completed :: TVar Bool
  }

newTraversalState :: STM (TraversalState)
newTraversalState = do
  active <- newTVar 0
  complete <- newTVar False
  return
    TraversalState
      { activeDirs = active,
        completed = complete
      }

traverseDirectory :: FileTQueue -> TraversalState -> FilePath -> IO ()
traverseDirectory fileQueue state path = do
  isFile <- doesFileExist path
  if isFile
    then atomically $ writeTQueue fileQueue path
    else do
      traverseDirectory' fileQueue state path []

traverseDirectory' :: FileTQueue -> TraversalState -> FilePath -> [Glob.Pattern] -> IO ()
traverseDirectory' fileQueue state path ignorePatterns = do
  atomically $ modifyTVar' (activeDirs state) (+ 1)

  subIgnorePatterns <- readGitignorePatterns path
  let currentIgnorePatterns = ignorePatterns <> subIgnorePatterns
  entries <- listDirectory path
  let filteredEntries = filter (not . isDefaultIgnoredFolder) entries
  let fullPaths = map (path </>) filteredEntries
  let notIgnored = filter (not . shouldIgnore currentIgnorePatterns) fullPaths

  mapM_ (processPath currentIgnorePatterns) notIgnored

  atomically $ do
    modifyTVar' (activeDirs state) (subtract 1)
    active <- readTVar (activeDirs state)
    when (active == 0) $ writeTVar (completed state) True
  where
    processPath :: [Glob.Pattern] -> FilePath -> IO ()
    processPath ignorePatterns fullPath = do
      isDir <- doesDirectoryExist fullPath
      if isDir
        then do
          _ <- forkIO $ traverseDirectory' fileQueue state fullPath ignorePatterns
          return ()
        else do
          atomically $ writeTQueue fileQueue fullPath

fileProcessor :: FileTQueue -> TraversalState -> TVar AggratedStats -> IO ()
fileProcessor fileQueue state statsVar = do
  mfile <- atomically $ tryReadTQueue fileQueue
  case mfile of
    Nothing -> do
      isCompleted <- atomically $ readTVar (completed state)
      if isCompleted
        then return ()
        else threadDelay 1000 >> fileProcessor fileQueue state statsVar
    Just file -> do
      stats <- processFile file
      atomically $ modifyTVar' statsVar (<> stats)
      fileProcessor fileQueue state statsVar

countLinesOfCode :: FilePath -> IO AggratedStats
countLinesOfCode rootPath = do
  numCores <- getNumCapabilities

  fileQueue <- newTQueueIO

  state <- atomically $ newTraversalState

  statsVar <- newTVarIO mempty

  workers <- replicateM numCores (async $ fileProcessor fileQueue state statsVar)

  _ <- forkIO $ traverseDirectory fileQueue state rootPath

  mapM_ wait workers

  finalStats <- readTVarIO statsVar
  return $! force finalStats
