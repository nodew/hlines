{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLines.Streamly where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName)
import qualified System.FilePath.Glob as Glob
import System.IO (Handle, IOMode(..), withFile, hSetEncoding, utf8, hIsEOF, hGetLine, hClose, openFile)
import Control.Exception (try, SomeException, bracket)
import Control.Monad.IO.Class (liftIO)

-- Streamly imports (updated)
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Fold (Fold)

import HLines.Types
import HLines.Languages
import HLines.Utils
import Control.DeepSeq (force)

-- Stream files from the filesystem
discoverFiles :: FilePath -> [Glob.Pattern] -> Stream IO FilePath
discoverFiles rootPath ignorePatterns = processPath rootPath
  where
    processPath :: FilePath -> Stream IO FilePath
    processPath path = Stream.handle (\(_ :: SomeException) -> pure $ Stream.fromList []) $
        Stream.concatMapM processDirectory $ Stream.fromList [path]
      where
        processDirectory :: FilePath -> IO (Stream IO FilePath)
        processDirectory p = handleIOErrors $ do
            isFile <- doesFileExist p
            isDir <- doesDirectoryExist p
            
            if isFile
                then pure $ Stream.fromList [p]
                else if isDir
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
                        pure $ Stream.fromList files `Stream.append` 
                               Stream.concatMap processPath (Stream.fromList dirs)
                    else pure $ Stream.fromList []
          where
            handleIOErrors :: IO a -> IO a
            handleIOErrors action = try action >>= \case
                Left (e :: SomeException) -> pure $ error $ "Error processing " ++ p ++ ": " ++ show e
                Right result -> pure result

-- Stream-based line processing with line-by-line reading
countLines :: FilePath -> Language -> Stream IO FileStats
countLines filepath lang = Stream.bracketIO
    (openFile filepath)
    hClose
    processLinesFromHandle
  where
    openFile path = do
        exists <- doesFileExist path
        if exists
            then do
                h <- openTextFile path
                return h
            else error $ "File not found: " ++ path

    openTextFile path = do
        h <- System.IO.openFile path ReadMode
        -- hSetEncoding h utf8
        return h

    processLinesFromHandle handle = Stream.unfoldrM go ([], handle)
      where
        go (activeBlocks, handle) = do
            eof <- hIsEOF handle
            if eof
                then return Nothing
                else do
                    line <- BSC.hGetLine handle
                    
                    let (currentStats, newActiveBlocks) = processLine lang (mempty, activeBlocks) line

                    return $! force Just (currentStats, (newActiveBlocks, handle))

-- Process files concurrently and aggregate stats
countLinesOfCode :: FilePath -> IO AggratedStats
countLinesOfCode rootPath = do
    numCores <- getNumCapabilities
    
    -- Process files with basic concurrency
    Stream.fold foldStats $ 
        Stream.mapM (\f -> processFile f) $
        Stream.filter (isJust . identifyLanguage) $
        discoverFiles rootPath []
  where
    foldStats :: Fold IO AggratedStats AggratedStats
    foldStats = Fold.foldl' (<>) mempty

    processFile :: FilePath -> IO AggratedStats
    processFile file = do
        let lang = identifyLanguage file
        case lang of
            Nothing -> return mempty
            Just lang -> do
                fileStats <- Stream.fold (Fold.foldl' (<>) mempty) $ countLines file lang
                return $! singleAggratedStats (name lang) fileStats
