{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module HLines.Internal
  ( readLines
  , mergeByLang
  ) where

import HLines.Type
import HLines.Language
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import System.IO
import System.IO.Error (IOError)
import Control.Exception (catch)
import System.FilePath (takeExtension)
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Control.Lens
import Prelude hiding (lines)
import Data.Word
import Control.Monad

lines :: BS.ByteString -> [BS.ByteString]
lines = C.split '\n'

readLines :: FilePath -> IO (Language, Comment, Lines)
readLines fp = do
  content <- liftIO $ BS.readFile fp `catch` (\(e :: IOError) -> return $ "")
  guard $ not $ BS.null content
  let ext = takeExtension fp
      lang = getLangFromExt ext
      comment = getCommentStyle lang
      lines' = lines content
  return (lang, comment, lines')

mergeCount :: Count -> Count -> Count
mergeCount Count {..} count = count & total +~ _total & code +~ _code & blank +~ _blank & comment +~ _comment

mergeByLang :: Map.Map Language Count -> (Language, Count) -> Map.Map Language Count
mergeByLang m (lang, count) =
  case Map.lookup lang m of
    Nothing -> Map.insert lang count m
    Just p -> Map.adjust (mergeCount count) lang m
