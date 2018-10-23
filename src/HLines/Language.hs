module HLines.Language where

import Data.Text as T
import HLines.Type

getLangFromExt :: T.Text -> Language
getLangFromExt ext
  | elem ext [".js", ".jsx"] = JavaScript
  | elem ext [".ts", ".tsx"] = TypeScript
  | elem ext [".hs"]       = Haskell
  | otherwise = Unknown

getCommentStyle :: Language -> Comment
getCommentStyle JavaScript = Comment ["//"] [("/*", "*/")]
getCommentStyle TypeScript = Comment ["//"] [("/*", "*/")]
getCommentStyle Haskell = Comment ["--"] [("{-", "-}")]
getCommentStyle _ = Comment [] []
