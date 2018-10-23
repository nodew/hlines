{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module HLines.Type where

import Control.Lens
import Data.Data
import Data.Text as T
import Data.Typeable

data Options = Options
  { files :: [FilePath]
  , ignore_ :: String
  , language :: [String]
  } deriving (Show, Data, Typeable)

data Count = Count
  { _code :: Int
  , _blank :: Int
  , _comment :: Int
  , _total :: Int
  } deriving (Show)

makeLenses ''Count

data Language
  = JavaScript
  | TypeScript
  | Haskell
  | Unknown
  deriving (Show, Eq)

data Comment = Comment
  { single :: [T.Text]
  , multi :: [(T.Text, T.Text)]
  }

type Result = [(Language, Count)]
