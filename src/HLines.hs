{-# LANGUAGE OverloadedStrings #-}
module HLines where

import System.Environment (getArgs)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, withCurrentDirectory)
import System.FilePath (takeExtension, (</>), takeFileName, takeExtensions)
import Control.Monad (filterM, foldM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap as HashMap
import qualified System.FilePath.Glob as Glob
import Data.Maybe (fromMaybe, isJust)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Parallel.Strategies (using, parMap, rdeepseq)
import Control.DeepSeq (NFData, rnf)
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as F

import HLines.Types
import HLines.Languages
import Data.Text.Array (new)
import Control.Concurrent (getNumCapabilities)
import qualified Control.Monad as F

identifyLanguage :: FilePath -> Maybe Language
identifyLanguage filepath = 
    let ext = T.pack $ takeExtension filepath
        allExt = T.pack $ takeExtensions filepath
        fileName = T.pack $ takeFileName filepath
        matchExt = filter (\lang -> any (\e -> e == allExt || e == ext || e == fileName) (extensions lang)) languages
    in case matchExt of
        (x:_) -> Just x
        [] -> Nothing  -- Unkown language, just ignore

-- Parse .gitignore file and get patterns
readGitignorePatterns :: FilePath -> IO [Glob.Pattern]
readGitignorePatterns dir = do
    let gitignorePath = dir </> ".gitignore"
    exists <- doesFileExist gitignorePath
    if exists
        then do
            content <- TIO.readFile gitignorePath
            let patterns = map (Glob.compile . T.unpack) $ filter isValidPattern $ T.lines content
            return patterns
        else return []
  where
    isValidPattern line = not (T.null line) && not ("#" `T.isPrefixOf` line)

-- Check if a file should be ignored based on .gitignore patterns
shouldIgnore :: [Glob.Pattern] -> FilePath -> Bool
shouldIgnore patterns path = any (\pattern -> Glob.match pattern path) patterns

-- Count lines in a file
countLines :: FilePath -> Language -> IO FileStats
countLines filepath lang = do
    exists <- doesFileExist filepath
    if exists
        then do
            content <- TIO.readFile filepath
            let result = countFileLines (T.lines content) lang [] 
            -- Force evaluation to prevent memory leaks
            return $! result
        else return mempty

-- Enhanced line counting with the refactored structure
countFileLines :: [Text] -> Language -> ActiveBlockComments -> FileStats
countFileLines [] _ _ = mempty
countFileLines (line:rest) lang activeBlocks =
    let trimmedLine = trim line
        isBlank = T.null trimmedLine
        
        -- Check if we're in any block comment
        (isInBlockComment, newActiveBlocks) = 
            if null activeBlocks
                then checkForNewBlockComment trimmedLine (multiLineComments lang)
                else checkForEndBlockComment trimmedLine activeBlocks
                
        -- Check for line comments if not in block comment
        isLineComment = not isInBlockComment && not isBlank && 
                        any (`T.isPrefixOf` trimmedLine) (lineComments lang)
                
        lineType 
            | isBlank = Blank
            | isInBlockComment || isLineComment = Comment
            | otherwise = Code

        currentStats = case lineType of
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
                           then (True, [])  -- Block comment starts and ends on same line
                           else (True, [style])  -- Block comment starts but doesn't end
               [] -> (False, []) -- This case is already handled by the outer if, but adding for completeness

-- Check if a line ends an active block comment
checkForEndBlockComment :: Text -> ActiveBlockComments -> (Bool, ActiveBlockComments)
checkForEndBlockComment _ [] = (False, [])
checkForEndBlockComment line (style:rest) =
    if blockEnd style `T.isSuffixOf` line
        then (True, rest)  -- This block comment ended
        else (True, style:rest)  -- Still in block comment
        
-- Helper functions
trim :: Text -> Text
trim = T.strip

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
        langStats = LanguageStats 1 (fileLines stats) (fileBlank stats) (fileComment stats) (fileCode stats)

-- Process a single file and return its stats mapped to language
processFile :: FilePath -> IO AggratedStats
processFile file = do
    let lang = identifyLanguage file
    case lang of
        Nothing -> return mempty
        Just lang -> do
            stats <- countLines file lang
            return $! singleAggratedStats (name lang) stats

getFilePaths :: FilePath -> [Glob.Pattern] -> IO [FilePath]
getFilePaths path ignorePatterns = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    
    if isFile
        then return [path]
        else if isDir
            then do
                subIgnorePatterns <- readGitignorePatterns path
                currentIgnorePatterns <- return $ ignorePatterns <> subIgnorePatterns
                entries <- listDirectory path
                let fullPaths = map (path </>) entries
                let notIgnored = filter (not . shouldIgnore currentIgnorePatterns) fullPaths
                
                filePaths <- filterM doesFileExist notIgnored
                dirPaths <- filterM doesDirectoryExist notIgnored

                -- Traverse subdirectories concurrently
                subDirFiles <- mapConcurrently (\subPath -> getFilePaths subPath currentIgnorePatterns) dirPaths

                return $ filePaths ++ concat subDirFiles

        else return mempty

countLinesOfCode :: FilePath -> IO AggratedStats
countLinesOfCode rootPath = do
    files <- getFilePaths rootPath []
    S.fold (F.foldlM' (\a b -> return $ a <> b) mempty) $               -- Fold with monadic combine function
      S.maxThreads (-1) $                  -- Use all available cores
      S.mapM processFile $               -- Process each file
      S.fromList files               -- Input stream of files

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
