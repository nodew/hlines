{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module HLines.Internal where

import HLines.Type
import HLines.Language
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.IO.Error (IOError)
import Control.Exception (catch)
import System.FilePath (takeExtension)
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Control.Lens

readLines :: MonadIO m => FilePath -> m (Language, Comment, Lines)
readLines fp = do
  content <- liftIO $ T.readFile fp `catch` (\(e :: IOError) -> return $ "")
  let ext = takeExtension fp
      lang = getLangFromExt $ T.pack ext
      comment = getCommentStyle lang
      lines' = T.lines content
  return (lang, comment, lines')

mergeCount :: Count -> Count -> Count
mergeCount Count {..} count = count & total +~ _total & code +~ _code & blank +~ _blank & comment +~ _comment

mergeByLang :: Map.Map Language Count -> (Language, Count) -> Map.Map Language Count
mergeByLang m (lang, count) =
  case Map.lookup lang m of
    Nothing -> Map.insert lang count m
    Just p -> Map.adjust (mergeCount count) lang m
