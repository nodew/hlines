{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HLines.Utils where

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

-- Split a list into chunks of specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

identifyLanguage :: FilePath -> Maybe Language
identifyLanguage filepath = 
    let ext = T.pack $ takeExtension filepath
        allExt = T.pack $ takeExtensions filepath
        fileName = T.pack $ takeFileName filepath
    in getLanguageFromExtension allExt           -- Try full extension first - e.g. .tar.gz
       <|> getLanguageFromExtension ext          -- Try simple extension - e.g. .hs
       <|> getLanguageFromExtension fileName     -- Try filename - e.g. Dockerfile

readGitignorePatterns :: FilePath -> IO [Glob.Pattern]
readGitignorePatterns dir = do
    let gitignorePath = dir </> ".gitignore"
    exists <- doesFileExist gitignorePath
    if exists
        then do
            content <- withFile gitignorePath ReadMode $ \h -> do
                hSetEncoding h utf8
                TIO.hGetContents h
            let patterns = map (Glob.compile . T.unpack . unifiedPattern) $ filter isValidPattern $ T.lines content
            return $! patterns
        else return []
  where
    isValidPattern line = not (T.null line) && not ("#" `T.isPrefixOf` line)
    unifiedPattern pattern = if T.last pattern == '/' then T.init pattern else pattern

defaultIgnoredFolders :: [Text]
defaultIgnoredFolders = [
    ".git", 
    ".svn", 
    ".hg", 
    ".idea", 
    ".vscode", 
    ".vs",
    ".stack-work", 
    ".cabal-sandbox",
    "dist",
    "node_modules",
    "bin",
    "obj"
    ]

isDefaultIgnoredFolder :: FilePath -> Bool
isDefaultIgnoredFolder path = any ((==) (T.pack path)) defaultIgnoredFolders

-- Check if a file should be ignored based on .gitignore patterns
shouldIgnore :: [Glob.Pattern] -> FilePath -> Bool
shouldIgnore patterns path = any (\pattern -> Glob.match pattern path) patterns

-- Check if a line starts a new block comment
checkForNewBlockComment :: Text -> [BlockCommentStyle] -> (Bool, ActiveBlockComments)
checkForNewBlockComment line styles =
    let possibleStarts = [style | style <- styles, blockStart style `T.isPrefixOf` line]
    in if null possibleStarts
       then (False, [])
       else 
           case possibleStarts of
               (style:_) -> if blockEnd style `T.isSuffixOf` dropPrefix (blockStart style) line
                           then (True, [])       -- Block comment starts and ends on same line
                           else (True, [style])  -- Block comment starts but doesn't end
               [] -> (False, []) -- This case is already handled by the outer if, but adding for completeness

-- Check if a line ends an active block comment
checkForEndBlockComment :: Text -> ActiveBlockComments -> (Bool, ActiveBlockComments)
checkForEndBlockComment _ [] = (False, [])
checkForEndBlockComment line (style:rest) =
    if blockEnd style `T.isSuffixOf` line
        then (True, rest)         -- This block comment ended
        else (True, style:rest)   -- Still in block comment

dropPrefix :: Text -> Text -> Text
dropPrefix prefix str = 
    if prefix `T.isPrefixOf` str
        then T.drop (T.length prefix) str
        else str

singleAggratedStats :: Text -> FileStats -> AggratedStats
singleAggratedStats lang stats = AggratedStats
    { byLanguage = MergeMap $ HashMap.singleton lang langStats
    , totalStats = stats
    }
    where
        !langStats = LanguageStats 1 (fileLines stats) (fileBlank stats) (fileComment stats) (fileCode stats)

-- Format the results
formatResults :: AggratedStats -> Text
formatResults results = 
    T.concat [
        "Language Stats:\n",
        "----------------\n",
        T.intercalate "\n" (map formatLangStats (HashMap.toList (getMergeMap $ byLanguage results))),
        "\n\nTotal Stats:\n",
        "----------------\n",
        formatTotalStats (totalStats results)
    ]
  where
    formatLangStats (lang, stats) = T.concat [
        lang, ":\n",
        "  Files: ", T.pack (show (fileCount stats)), "\n",
        "  Lines of Code: ", T.pack (show (langCode stats)), "\n",
        "  Lines of Comments: ", T.pack (show (langComment stats)), "\n",
        "  Blank Lines: ", T.pack (show (langBlank stats)), "\n",
        "  Total Lines: ", T.pack (show (langCode stats + langComment stats + langBlank stats))
        ]
        
    formatTotalStats stats = T.concat [
        "Lines of Code: ", T.pack (show (fileCode stats)), "\n",
        "Lines of Comments: ", T.pack (show (fileComment stats)), "\n",
        "Blank Lines: ", T.pack (show (fileBlank stats)), "\n",
        "Total Lines: ", T.pack (show (fileCode stats + fileComment stats + fileBlank stats))
        ]