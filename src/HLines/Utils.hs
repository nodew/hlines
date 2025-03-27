{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module HLines.Utils where

import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.FilePath (takeExtension, (</>), takeFileName, takeExtensions)
import Control.Monad (filterM, foldM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.ByteString (ByteString)
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
import Control.Concurrent (getNumCapabilities)
import qualified Control.Monad as F
import Control.Exception (evaluate, SomeException, try)
import System.IO (IOMode(..), withFile, hSetEncoding, utf8, hIsEOF, Handle)
import qualified Streamly.Data.Stream as Stream

import HLines.Types
import HLines.Languages
import Control.Monad.IO.Class (MonadIO (..))

-- Split a list into chunks of specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

identifyLanguage :: FilePath -> Maybe Language
identifyLanguage filepath = 
    let ext = BSC.pack $ safeTail $ takeExtension filepath
        allExt = BSC.pack $ safeTail $ takeExtensions filepath
        fileName = BSC.pack $ takeFileName filepath
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
                BSC.hGetContents h
            let patterns = map (Glob.compile . BSC.unpack . unifiedPattern) $ filter isValidPattern $ BSC.lines content
            return $! patterns
        else return []
  where
    isValidPattern line = not (BS.null line) && not ("#" `BS.isPrefixOf` line)
    unifiedPattern pattern = if BSC.last pattern == '/' then BS.init pattern else pattern

defaultIgnoredFolders :: [ByteString]
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
isDefaultIgnoredFolder path = any ((==) (BSC.pack path)) defaultIgnoredFolders

-- Check if a file should be ignored based on .gitignore patterns
shouldIgnore :: [Glob.Pattern] -> FilePath -> Bool
shouldIgnore patterns path = any (\pattern -> Glob.match pattern path) patterns

-- Check if a line starts a new block comment
checkForNewBlockComment :: ByteString -> [BlockCommentStyle] -> (Bool, ActiveBlockComments)
checkForNewBlockComment line styles =
    let possibleStarts = [style | style <- styles, blockStart style `BS.isPrefixOf` line]
    in if null possibleStarts
       then (False, [])
       else 
           case possibleStarts of
               (style:_) -> if blockEnd style `BS.isSuffixOf` dropPrefix (blockStart style) line
                           then (True, [])       -- Block comment starts and ends on same line
                           else (True, [style])  -- Block comment starts but doesn't end
               [] -> (False, []) -- This case is already handled by the outer if, but adding for completeness

-- Check if a line ends an active block comment
checkForEndBlockComment :: ByteString -> ActiveBlockComments -> (Bool, ActiveBlockComments)
checkForEndBlockComment _ [] = (False, [])
checkForEndBlockComment line (style:rest) =
    if blockEnd style `BS.isSuffixOf` line
        then (True, rest)         -- This block comment ended
        else (True, style:rest)   -- Still in block comment

dropPrefix :: ByteString -> ByteString -> ByteString
dropPrefix prefix str = 
    if prefix `BS.isPrefixOf` str
        then BS.drop (BS.length prefix) str
        else str

singleAggratedStats :: ByteString -> FileStats -> AggratedStats
singleAggratedStats lang stats = AggratedStats
    { byLanguage = MergeMap $ HashMap.singleton lang langStats
    , totalStats = stats
    }
    where
        !langStats = LanguageStats 1 (fileLines stats) (fileBlank stats) (fileComment stats) (fileCode stats)

processLine :: Language -> (FileStats, ActiveBlockComments) -> ByteString -> (FileStats, ActiveBlockComments)
processLine lang (stats, activeBlocks) line =
    let !trimmedLine = BSC.strip line
        !isBlank = BS.null trimmedLine
        
        -- Check if we're in any block comment
        (!isInBlockComment, !newActiveBlocks) = 
            if null activeBlocks
                then checkForNewBlockComment trimmedLine (multiLineComments lang)
                else checkForEndBlockComment trimmedLine activeBlocks
                
        -- Check for line comments if not in block comment
        !isLineComment = not isInBlockComment && not isBlank && 
                        any (`BS.isPrefixOf` trimmedLine) (lineComments lang)
                
        !lineType 
            | isBlank = Blank
            | isInBlockComment || isLineComment = Comment
            | otherwise = Code

        !currentStats = case lineType of
            Blank -> FileStats 1 1 0 0
            Comment -> FileStats 1 0 1 0
            Code -> FileStats 1 0 0 1

    in  (stats <> currentStats, newActiveBlocks)

processLine' :: Language -> (FileStats, ActiveBlockComments) -> BL.ByteString -> (FileStats, ActiveBlockComments)
processLine' lang (stats, activeBlocks) line = processLine lang (stats, activeBlocks) (BL.toStrict line)

countLines :: FilePath -> Language -> IO FileStats
countLines filepath lang = do
    exists <- doesFileExist filepath
    if exists
        then do
            contents <- BLC.readFile filepath
            return $! force fst $ foldl (processLine' lang) (mempty, []) (BLC.lines contents)
        else return mempty

processFile :: FilePath -> IO AggratedStats
processFile file = do
    let lang = identifyLanguage file
    case lang of
        Nothing -> return mempty
        Just lang -> do
            stats <- countLines file lang
            let !result = singleAggratedStats (name lang) stats
            return $! force result

-- Format the results
formatResults :: AggratedStats -> ByteString
formatResults results = 
    BS.concat [
        "Language Stats:\n",
        "----------------\n",
        BS.intercalate "\n" (map formatLangStats (HashMap.toList (getMergeMap $ byLanguage results))),
        "\n\nTotal Stats:\n",
        "----------------\n",
        formatTotalStats (totalStats results)
    ]
  where
    formatLangStats (lang, stats) = BS.concat [
        lang, ":\n",
        "  Files: ", BSC.pack (show (fileCount stats)), "\n",
        "  Lines of Code: ", BSC.pack (show (langCode stats)), "\n",
        "  Lines of Comments: ", BSC.pack (show (langComment stats)), "\n",
        "  Blank Lines: ", BSC.pack (show (langBlank stats)), "\n",
        "  Total Lines: ", BSC.pack (show (langCode stats + langComment stats + langBlank stats))
        ]
        
    formatTotalStats stats = BS.concat [
        "Lines of Code: ", BSC.pack (show (fileCode stats)), "\n",
        "Lines of Comments: ", BSC.pack (show (fileComment stats)), "\n",
        "Blank Lines: ", BSC.pack (show (fileBlank stats)), "\n",
        "Total Lines: ", BSC.pack (show (fileCode stats + fileComment stats + fileBlank stats))
        ]
