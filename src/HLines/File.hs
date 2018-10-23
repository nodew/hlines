{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module HLines.File where

import Control.Exception (catch)
import Control.Lens
import Control.Monad.State
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath ((</>), takeExtension)
import System.IO
import System.IO.Error (IOError)

import HLines.Language
import HLines.Type

type CountState = State ([String], Count, String)

trimLeft :: T.Text -> T.Text
trimLeft = T.dropWhile isSpace

countLines :: FilePath -> IO (FilePath, Language, Count)
countLines file = do
  contents <- T.readFile file `catch` (\(_ :: IOError) -> return "")
  let ext = takeExtension file
      lines' = T.lines contents
      initCount = Count 0 0 0 0
      lang = getLangFromExt $ T.pack ext
      comments = getCommentStyle lang
      (count, _) = foldl (countLines' comments) (initCount, Nothing) lines'
  return (file, lang, count)

countLines' ::
     Comment -> (Count, Maybe T.Text) -> T.Text -> (Count, Maybe T.Text)
countLines' (Comment [] []) (count, _) _ = (count & total +~ 1 & code +~ 1, Nothing)
countLines' Comment {..} (count, start) line =
  case line' of
    "" -> (count' & blank +~ 1, start)
    _ ->
      case start of
        Nothing ->
          if (not $ null single) && (and $ map (\c -> c `T.isPrefixOf` line') single)
            then (count' & comment +~ 1, Nothing)
            else let multi' = find (\c -> fst c `T.isPrefixOf` line') multi
                  in case multi' of
                       Nothing -> (count' & code +~ 1, Nothing)
                       Just m ->
                         if snd m `T.isInfixOf` line'
                           then (count & comment +~ 1, Nothing)
                           else (count & comment +~ 1, Just $ fst m)
        Just start' ->
          let end = lookup start' multi
           in case end of
                Nothing -> (count', Nothing)
                Just end' ->
                  if end' `T.isInfixOf` line'
                    then (count & comment +~ 1, Nothing)
                    else (count' & comment +~ 1, start)
  where
    line' = trimLeft line
    count' = count & total +~ 1

dirWalk :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
dirWalk filepath testFunc = do
  isDir <- doesDirectoryExist filepath
  isFile <- doesFileExist filepath
  if isDir
    then do
      files <- listDirectory filepath
      files' <- mapM (\file -> dirWalk (filepath </> file) testFunc) files
      return $ concat files'
    else if isFile
           then return [filepath]
           else return []

dirWalkDefault :: FilePath -> IO [FilePath]
dirWalkDefault = flip dirWalk (const True)
