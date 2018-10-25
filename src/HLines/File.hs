{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module HLines.File
  ( countLines
  , readContent
  ) where

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

trimLeft :: T.Text -> T.Text
trimLeft = T.dropWhile isSpace

initCount :: Count
initCount = Count 0 0 0 0

readContent :: FilePath -> IO (Language, T.Text)
readContent file = do
  content <- T.readFile file `catch` (\(_ :: IOError) -> return "")
  let ext = takeExtension file
      lang = getLangFromExt $ T.pack ext
  return (lang, content)

countLines :: Language -> T.Text -> Count
countLines lang content =
  let lines' = T.lines content
      comments = getCommentStyle lang
   in fst $ foldl (countLines' comments) (initCount, Nothing) lines'

countLines' :: Comment -> (Count, Maybe T.Text) -> T.Text -> (Count, Maybe T.Text)
countLines' comment (count, start) line = countLines'' comment (count', start) line'
  where
    line' = trimLeft line
    count' = count & total +~ 1

countLines'' :: Comment -> (Count, Maybe T.Text) -> T.Text -> (Count, Maybe T.Text)
countLines'' _ (count, open) "" = (count & blank +~ 1, open)
countLines'' (Comment [] _) (count, Nothing) _ = (count & code +~ 1, Nothing)
countLines'' Comment {..} (count, Nothing) line =
  if and $ map (\c -> c `T.isPrefixOf` line) single
    then (count & comment +~ 1, Nothing)
    else case find (\c -> fst c `T.isPrefixOf` line) multi of
           Nothing -> (count & code +~ 1, Nothing)
           Just m ->
             if snd m `T.isInfixOf` line
               then (count & comment +~ 1, Nothing)
               else (count & comment +~ 1, Just $ fst m)
countLines'' Comment {..} (count, Just open) line = (count & comment +~ 1, open')
  where
    open' =
      case lookup open multi of
        Just _close ->
          if _close `T.isInfixOf` line
            then Nothing
            else Just open
        _ -> Nothing
