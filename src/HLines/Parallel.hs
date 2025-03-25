{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HLines.Parallel where

import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.FilePath (takeExtension, (</>), takeFileName, takeExtensions)
import Control.Monad (filterM, foldM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified System.FilePath.Glob as Glob
import Data.Maybe (fromMaybe, isJust)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Parallel.Strategies (using, parMap, rdeepseq)
import Control.DeepSeq (NFData, rnf, force)
import qualified Streamly.Data.Stream.Prelude as SP
import qualified Streamly.Data.Fold.Prelude as SF
import Control.Applicative ((<|>))
import Data.Text.Array (new)
import Control.Concurrent (getNumCapabilities)
import qualified Control.Monad as F
import Control.Exception (evaluate, SomeException, try)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8)

import HLines.Types
import HLines.Languages
import HLines.Utils

-- Count lines in a file
countLines :: FilePath -> Language -> IO FileStats
countLines filepath lang = do
    exists <- doesFileExist filepath
    if exists
        then do
            result <- tryReadFile filepath
            case result of
                Left err -> do
                    putStrLn $ "Warning: Could not read file " ++ filepath ++ ": " ++ show err
                    return mempty
                Right content -> do
                    let !result = countFileLines (T.lines content) lang [] 
                    return $! force result
        else return mempty

tryReadFile :: FilePath -> IO (Either SomeException Text)
tryReadFile filepath = try $ withFile filepath ReadMode $ \h -> do
    hSetEncoding h utf8
    TIO.hGetContents h

-- Enhanced line counting with the refactored structure
countFileLines :: [Text] -> Language -> ActiveBlockComments -> FileStats
countFileLines [] _ _ = mempty
countFileLines (line:rest) lang activeBlocks =
    let !trimmedLine = T.strip line
        !isBlank = T.null trimmedLine
        
        -- Check if we're in any block comment
        (!isInBlockComment, !newActiveBlocks) = 
            if null activeBlocks
                then checkForNewBlockComment trimmedLine (multiLineComments lang)
                else checkForEndBlockComment trimmedLine activeBlocks
                
        -- Check for line comments if not in block comment
        !isLineComment = not isInBlockComment && not isBlank && 
                        any (`T.isPrefixOf` trimmedLine) (lineComments lang)
                
        !lineType 
            | isBlank = Blank
            | isInBlockComment || isLineComment = Comment
            | otherwise = Code

        !currentStats = case lineType of
            Blank -> FileStats 1 1 0 0
            Comment -> FileStats 1 0 1 0
            Code -> FileStats 1 0 0 1

    in currentStats <> countFileLines rest lang newActiveBlocks


-- Process a single file and return its stats mapped to language
processFile :: FilePath -> IO AggratedStats
processFile file = do
    let lang = identifyLanguage file
    case lang of
        Nothing -> return mempty
        Just lang -> do
            stats <- countLines file lang
            let !result = singleAggratedStats (name lang) stats
            return $! force result

getFilePaths :: FilePath -> [Glob.Pattern] -> IO [FilePath]
getFilePaths path ignorePatterns = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    
    if isFile
        then return [path]
        else if isDir
            then do
                subIgnorePatterns <- readGitignorePatterns path
                let !currentIgnorePatterns = ignorePatterns <> subIgnorePatterns
                entries <- listDirectory path
                let filteredEntries = filter (not . isDefaultIgnoredFolder) entries
                let fullPaths = map (path </>) (filteredEntries)
                
                let notIgnored = filter (not . shouldIgnore currentIgnorePatterns) fullPaths
                
                (filePaths, dirPaths) <- do
                    files <- filterM doesFileExist notIgnored
                    dirs <- filterM (\folder -> doesDirectoryExist folder) notIgnored
                    return (files, dirs)
                
                chunkSize <- (`max` 1) . (`div` 4) <$> getNumCapabilities
                let dirChunks = chunksOf chunkSize dirPaths
                
                subDirFilesChunks <- mapConcurrently (\chunk -> 
                    foldM (\acc dir -> do
                        files <- getFilePaths dir currentIgnorePatterns
                        return $! acc ++ files) [] chunk) dirChunks
                
                let !result = filePaths ++ concat subDirFilesChunks
                return $! force result
            else return []

countLinesOfCode :: FilePath -> IO AggratedStats
countLinesOfCode rootPath = do
    files <- getFilePaths rootPath []
    numCores <- getNumCapabilities

    let chunkSize = max 100 (length files `div` numCores)
    let fileChunks = chunksOf chunkSize files
    
    statsChunks <- mapConcurrently (\chunk -> do
        foldM (\acc file -> do
            stats <- processFile file
            let !newAcc = acc <> stats
            return $! force newAcc) mempty chunk) fileChunks
    
    let !finalStats = foldl (<>) mempty statsChunks
    
    return $! force finalStats
