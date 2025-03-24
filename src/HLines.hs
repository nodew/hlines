{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module HLines where

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

-- Efficient language identification using extension map
identifyLanguage :: FilePath -> Maybe Language
identifyLanguage filepath = 
    let ext = T.pack $ takeExtension filepath
        allExt = T.pack $ takeExtensions filepath
        fileName = T.pack $ takeFileName filepath
    in getLanguageFromExtension allExt           -- Try full extension first - e.g. .tar.gz
       <|> getLanguageFromExtension ext          -- Try simple extension - e.g. .hs
       <|> getLanguageFromExtension fileName     -- Try filename - e.g. Dockerfile

-- Parse .gitignore file and get patterns
readGitignorePatterns :: FilePath -> IO [Glob.Pattern]
readGitignorePatterns dir = do
    let gitignorePath = dir </> ".gitignore"
    exists <- doesFileExist gitignorePath
    if exists
        then do
            content <- withFile gitignorePath ReadMode $ \h -> do
                hSetEncoding h utf8
                TIO.hGetContents h >>= evaluate . force
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

-- Count lines in a file
countLines :: FilePath -> Language -> IO FileStats
countLines filepath lang = do
    exists <- doesFileExist filepath
    if exists
        then do
            result <- tryReadFile filepath
            case result of
                Left err -> do
                    -- 打印错误信息并跳过该文件
                    putStrLn $ "Warning: Could not read file " ++ filepath ++ ": " ++ show err
                    return mempty
                Right content -> do
                    -- 正常处理文件内容
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

-- Split a list into chunks of specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

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
