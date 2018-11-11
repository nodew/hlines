{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module HLines.Counter
  ( countLines
  ) where

import Control.Exception (catch)
import Control.Lens
import Control.Monad.Writer
import Data.List (any, elemIndices, find, foldl')
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (takeExtension)
import System.IO
import System.IO.Error (IOError)

import HLines.Language
import HLines.Type

initCount :: Count
initCount = Count 0 0 0 0

countLines :: Comment -> [T.Text] -> Count
countLines comments content = fst $ foldl' (countLine comments) (initCount, [] :: MultiComments) content

countLine :: Comment -> (Count, MultiComments) -> T.Text -> (Count, MultiComments)
countLine comment (count, multiStack) line = countLine' comment (count', multiStack) line'
  where
    line' = T.stripStart line
    count' = count & total +~ 1

countLine' :: Comment -> (Count, MultiComments) -> T.Text -> (Count, MultiComments)
-- Empty line
countLine' _ (count, stack) "" = (count & blank +~ 1, stack)
-- No Comment
countLine' (Comment [] []) (count, _) _ = (count & code +~ 1, [])
-- With single line comment only
countLine' (Comment single []) (count, _) line = (count', [])
  where
    count' =
      if prefixWithSingleComment single line
        then (count & comment +~ 1)
        else (count & code +~ 1)
-- Current line not in multiple line comment block
countLine' Comment {..} (count, []) line
  | prefixWithSingleComment single line = (count & comment +~ 1, [])
  | isWithComment =
    if and [0 `elem` sizes, hasCodeInLine]
      then (count & code +~ 1, stack)
      else (count & comment +~ 1, stack)
  | otherwise = (count & code +~ 1, stack)
  where
    (stack, sizes) = traverseLine multi [] line
    withMultiComment = find (\c -> fst c `T.isPrefixOf` line) multi
    isWithComment = isJust withMultiComment
    hasCodeInLine =
      if last sizes == 0
        then not $ snd (fromJust withMultiComment) `T.isSuffixOf` (T.stripEnd line)
        else False
-- Current line inside mulitple line comment block
countLine' Comment {..} (count, stack) line =
  if null stack' && hasCodeInLine
    then (count & code +~ 1, stack')
    else (count & comment +~ 1, stack')
  where
    (stack', sizes) = traverseLine multi stack line
    hasCodeInLine =
      or [length (elemIndices 0 sizes) > 1, last sizes == 0 && not (snd (last stack) `T.isSuffixOf` (T.stripEnd line))]

prefixWithSingleComment :: [T.Text] -> T.Text -> Bool
prefixWithSingleComment single line = any (\c -> c `T.isPrefixOf` line) single

traverseLine :: MultiComments -> MultiComments -> T.Text -> (MultiComments, [Int])
traverseLine multi stack line = runWriter $ go stack line
  where
    go, goMulti :: MultiComments -> T.Text -> Writer [Int] MultiComments
    go stack' "" = return stack'
    go [] line' = goMulti [] line'
    go stack'@(x:xs) line' =
      if (snd x `T.isPrefixOf` line')
        then do
          tell $ [length xs]
          go xs $ T.drop (T.length (snd x)) line'
        else goMulti stack' line'
    goMulti stack' line' =
      case find (\c -> fst c `T.isPrefixOf` line') multi of
        Nothing -> go stack' (T.drop 1 line')
        Just m -> do
          let s = m : stack'
          tell $ [length s]
          go s $ T.drop (T.length (fst m)) line'
