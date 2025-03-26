{-# LANGUAGE BangPatterns #-}

module HLines.Conduit where

import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName)
import Control.Exception (SomeException, try)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.Binary as CB
import qualified System.FilePath.Glob as Glob
import Control.Concurrent (getNumCapabilities)
import Conduit
import Control.DeepSeq (force)

import HLines.Types
import HLines.Languages
import HLines.Utils hiding (processFile)

countFileLinesConduit :: FilePath -> Language -> IO FileStats
countFileLinesConduit file lang = do
    exists <- doesFileExist file
    if not exists 
        then return mempty 
        else do
            (stats, _) <- runConduitRes $
                CB.sourceFile file
                .| CB.lines
                .| foldlC (processLine lang) (mempty, [])
            return $! force stats

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
