{-# LANGUAGE BangPatterns #-}

module HLines.Conduit where

import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>), takeFileName, takeExtensions)
import Control.Exception (evaluate, SomeException, try)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)
import qualified Data.Conduit.Binary as CB
import qualified Data.HashMap.Strict as HashMap
import qualified System.FilePath.Glob as Glob
import Control.Concurrent (getNumCapabilities)
import Conduit

import HLines.Types
import HLines.Languages
import HLines.Utils

countFileLinesConduit :: FilePath -> Language -> IO FileStats
countFileLinesConduit file lang = do
    exists <- doesFileExist file
    if not exists 
        then return mempty 
        else do
            (stats, _) <- runConduitRes $
                CB.sourceFile file
                .| CB.lines
                .| foldlC processLine (mempty, [])
            return stats
  where
    processLine (!acc, !activeBlocks) line =
        let !trimmed = BSC.strip line
            !isBlank = BS.null trimmed
            
            (!isInBlockComment, !newActiveBlocks) = 
                if null activeBlocks
                    then checkForNewBlockComment trimmed (multiLineComments lang)
                    else checkForEndBlockComment trimmed activeBlocks
            
            !isLineComment = not isInBlockComment && not isBlank && 
                                any (`BS.isPrefixOf` trimmed) (lineComments lang)

            !lineType 
                | isBlank = Blank
                | isInBlockComment || isLineComment = Comment
                | otherwise = Code

            !currentStats = case lineType of
                Blank -> FileStats 1 1 0 0
                Comment -> FileStats 1 0 1 0
                Code -> FileStats 1 0 0 1

        in (currentStats <> acc, newActiveBlocks)

processFile :: FilePath -> IO AggratedStats
processFile file = case identifyLanguage file of
    Nothing    -> return mempty
    Just lang  -> do
        stats <- countFileLinesConduit file lang
        return $! singleAggratedStats (name lang) stats

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

countLinesOfCode :: FilePath -> IO AggratedStats
countLinesOfCode root = do
    numCores <- getNumCapabilities
    runConduitRes $
        getFilePathsConduit root
        .| mapMC (liftIO . processFile)
        .| foldlC (<>) mempty
