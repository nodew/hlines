{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLines.Languages where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.FileEmbed (embedFile)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import Data.Maybe

import HLines.Types
import HLines.Types (BlockCommentStyle(BlockCommentStyle))

-- Language configuration

languageConfigsFile :: ByteString
languageConfigsFile = $(embedFile "languages.json")

data LanguageConfig = LanguageConfig
  { lcExtensions :: [Text],
    lcLineComments :: [Text],
    lcMultiLineComments :: Maybe [[Text]]
  }

type LanguageConfigs = Map.Map Text LanguageConfig

instance FromJSON LanguageConfig where
  parseJSON = withObject "LanguageConfig" $ \o -> do
    lcExtensions <- o .: "extensions"
    lcLineComments <- o .: "line_comment"
    lcMultiLineComments <- o .:? "multi_line"
    return LanguageConfig {..}

languageConfigs :: LanguageConfigs
languageConfigs = case eitherDecodeStrict languageConfigsFile of
  Left err -> error $ "Failed to parse language configs: " ++ err
  Right x -> x

languages :: [Language]
languages = flip map (Map.toList languageConfigs) $ \(k, v) -> 
  let multiLineComments = case (lcMultiLineComments v) of 
                            Just comments -> comments >>= \case
                              [start, end] -> return $ BlockCommentStyle (E.encodeUtf8 start) (E.encodeUtf8 end)
                              _ -> error "Only two multi-line comment delimiters are supported"
                            Nothing -> []
  in
    Language {
      name = E.encodeUtf8 k,
      extensions = map E.encodeUtf8 $ lcExtensions v,
      lineComments = map E.encodeUtf8 $ lcLineComments v,
      multiLineComments = multiLineComments
    }

extentionToLangMap :: HashMap.HashMap ByteString Language
extentionToLangMap = HashMap.fromList $ do
  lang <- languages
  ext <- extensions lang
  return (ext, lang)

-- Get a language configuration from a file extension
getLanguageFromExtension :: ByteString -> Maybe Language
getLanguageFromExtension ext = HashMap.lookup ext extentionToLangMap
