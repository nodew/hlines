{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module HLines.File ( countLines ) where

import Control.Exception (catch)
import Control.Lens
import Control.Monad.State
import Data.Char (isSpace)
import Data.List (find)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (takeExtension)
import System.IO
import System.IO.Error (IOError)

import HLines.Language
import HLines.Type

type CountState = State ([String], Count, String)

trimLeft :: T.Text -> T.Text
trimLeft = T.dropWhile isSpace

initCount :: Count
initCount = Count 0 0 0 0

countLines :: FilePath -> IO (FilePath, Language, Count)
countLines file = do
  contents <- T.readFile file `catch` (\(_ :: IOError) -> return "")
  let ext = takeExtension file
      lines' = T.lines contents
      lang = getLangFromExt $ T.pack ext
      comments = getCommentStyle lang
      (count, _) = foldl (countLines' comments) (initCount, Nothing) lines'
  return (file, lang, count)

countLines' :: Comment -> (Count, Maybe T.Text) -> T.Text -> (Count, Maybe T.Text)
countLines' comment (count, start) line = countLines'' comment (count', start) line'
   where
    line' = trimLeft line
    count' = count & total +~ 1
    
countLines'' :: Comment -> (Count, Maybe T.Text) -> T.Text -> (Count, Maybe T.Text)
countLines'' (Comment [] []) (count, _) _ = (count & code +~ 1, Nothing)
countLines'' _ (count, start) "" = (count & blank +~ 1, start)
countLines'' (Comment [] _) (count, Nothing) _ = (count & code +~ 1, Nothing)
countLines'' Comment{..} (count, Nothing) line =
  if and $ map (\c -> c `T.isPrefixOf` line) single
    then (count & comment +~ 1, Nothing)
    else case find (\c -> fst c `T.isPrefixOf` line) multi of
           Nothing -> (count & code +~ 1, Nothing)
           Just m ->
             if snd m `T.isInfixOf` line
               then (count & comment +~ 1, Nothing)
               else (count & comment +~ 1, Just $ fst m)
countLines'' Comment {..} (count, Just start) line =
  case lookup start multi of
    Nothing -> (count, Nothing)
    Just end ->
      if end `T.isInfixOf` line
        then (count & comment +~ 1, Nothing)
        else (count & comment +~ 1, Just start)
