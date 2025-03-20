{-# LANGUAGE OverloadedStrings #-}

module HLines.Types where

import Control.DeepSeq (NFData, rnf)
import Data.HashMap (Map)
import qualified Data.HashMap as HashMap
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Semigroup (Semigroup)

newtype MergeMap k v = MergeMap {getMergeMap :: Map k v} deriving (Show)

instance (Semigroup v, Eq k, Hashable k, Ord k) => Semigroup (MergeMap k v) where
  MergeMap x <> MergeMap y = MergeMap $ HashMap.unionWith (<>) x y

instance (Semigroup v, Eq k, Hashable k, Ord k) => Monoid (MergeMap k v) where
  mempty = MergeMap mempty
  mappend = (<>)

data LineType = Comment | Code | Blank deriving (Show, Eq)

-- Data structure to track file statistics

data FileStats = FileStats {
    fileLines :: Int,
    fileBlank :: Int,
    fileComment :: Int,
    fileCode :: Int
} deriving (Show, Eq)

instance NFData FileStats where
    rnf (FileStats l b c cd) = rnf l `seq` rnf b `seq` rnf c `seq` rnf cd

instance Semigroup FileStats where
    (FileStats l1 b1 c1 cd1) <> (FileStats l2 b2 c2 cd2) = FileStats (l1 + l2) (b1 + b2) (c1 + c2) (cd1 + cd2)

instance Monoid FileStats where
    mempty = FileStats 0 0 0 0

-- Data structure to track language statistics

data LanguageStats = LanguageStats {
    fileCount :: Int,
    langLines :: Int,
    langBlank :: Int,
    langComment :: Int,
    langCode :: Int
} deriving (Show, Eq)

instance NFData LanguageStats where
    rnf (LanguageStats f l b c cd) = rnf f `seq` rnf l `seq` rnf b `seq` rnf c `seq` rnf cd

instance Semigroup LanguageStats where
    (LanguageStats f1 l1 b1 c1 cd1) <> (LanguageStats f2 l2 b2 c2 cd2) = LanguageStats (f1 + f2) (l1 + l2) (b1 + b2) (c1 + c2) (cd1 + cd2)

-- Block comment style
data BlockCommentStyle = BlockCommentStyle {
    blockStart :: Text,
    blockEnd :: Text
} deriving (Show, Eq)

instance NFData BlockCommentStyle where
    rnf (BlockCommentStyle bs be) = rnf bs `seq` rnf be

-- Language configuration
data Language = Language {
    name :: Text,
    extensions :: [Text],
    lineComments :: [Text],
    multiLineComments :: [BlockCommentStyle]
}

instance NFData Language where
    rnf (Language n exts lc mlc) = rnf n `seq` rnf exts `seq` rnf lc `seq` rnf mlc

-- Data structure to track active block comments
type ActiveBlockComments = [BlockCommentStyle]  -- List of active block comments

data AggratedStats = AggratedStats {
    byLanguage :: MergeMap Text LanguageStats,
    totalStats :: FileStats
} deriving (Show)

instance Semigroup AggratedStats where
  (AggratedStats lang1 total1) <> (AggratedStats lang2 total2) = AggratedStats (lang1 <> lang2) (total1 <> total2)

instance Monoid AggratedStats where
    mempty = AggratedStats mempty mempty
    mappend = (<>)