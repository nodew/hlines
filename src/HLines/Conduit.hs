{-# LANGUAGE BangPatterns #-}

module HLines.Conduit where

import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>), takeFileName, takeExtensions)
import Control.Exception (evaluate, SomeException, try)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HashMap
import qualified System.FilePath.Glob as Glob
import Control.Concurrent (getNumCapabilities)
import Conduit

import HLines.Types
import HLines.Languages
import HLines.Utils

countLinesConduit :: FilePath -> Language -> IO FileStats
countLinesConduit file lang = do
    exists <- doesFileExist file
    if not exists then return mempty else
        runConduitRes $
            sourceFile file
            .| decodeUtf8C
            .| linesUnboundedC
            .| foldlC countLineTypes mempty
  where
    countLineTypes !acc line =
        let trimmed = T.strip line
        in if T.null trimmed
            then acc <> FileStats 1 1 0 0
            else if any (`T.isPrefixOf` trimmed) (lineComments lang)
                then acc <> FileStats 1 0 1 0
                else acc <> FileStats 1 0 0 1

processFile :: FilePath -> IO AggratedStats
processFile file = case identifyLanguage file of
    Nothing    -> return mempty
    Just lang  -> do
        stats <- countLinesConduit file lang
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
