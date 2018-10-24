module HLines.Language where

import Data.Text as T
import HLines.Type

getLangFromExt :: T.Text -> Language
getLangFromExt ext
  | elem ext [".js", ".jsx"] = JavaScript
  | elem ext [".ts", ".tsx"] = TypeScript
  | elem ext [".hs"] = Haskell
  | elem ext [".html", ".htm"] = HTML
  | elem ext [".php"] = PHP
  | otherwise = Unknown

getCommentStyle :: Language -> Comment
getCommentStyle JavaScript = Comment ["//"] [("/*", "*/")]
getCommentStyle TypeScript = Comment ["//"] [("/*", "*/")]
getCommentStyle Haskell = Comment ["--"] [("{-", "-}")]
getCommentStyle HTML = Comment [] [("<!--", "-->")]
getCommentStyle PHP = Comment ["#", "//"] [("/*", "*/")]
getCommentStyle _ = Comment [] []

