{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HLines.Types where

import Control.DeepSeq (NFData, rnf)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Semigroup (Semigroup)
import GHC.Generics (Generic)

newtype MergeMap k v = MergeMap {getMergeMap :: HashMap.HashMap k v} deriving (Show, Generic, NFData)

instance (Semigroup v, Eq k, Hashable k) => Semigroup (MergeMap k v) where
  MergeMap x <> MergeMap y = MergeMap $ HashMap.unionWith (<>) x y

instance (Semigroup v, Eq k, Hashable k) => Monoid (MergeMap k v) where
  mempty = MergeMap mempty
  mappend = (<>)

data LineType = Comment | Code | Blank deriving (Show, Eq)

data FileStats = FileStats {
    fileLines :: !Int,
    fileBlank :: !Int,
    fileComment :: !Int,
    fileCode :: !Int
} deriving (Show, Eq, Generic)

instance NFData FileStats where
    rnf (FileStats l b c cd) = rnf l `seq` rnf b `seq` rnf c `seq` rnf cd

instance Semigroup FileStats where
    (FileStats l1 b1 c1 cd1) <> (FileStats l2 b2 c2 cd2) = 
        let !l = l1 + l2
            !b = b1 + b2
            !c = c1 + c2
            !cd = cd1 + cd2
        in FileStats l b c cd

instance Monoid FileStats where
    mempty = FileStats 0 0 0 0

data LanguageStats = LanguageStats {
    fileCount :: !Int,
    langLines :: !Int,
    langBlank :: !Int,
    langComment :: !Int,
    langCode :: !Int
} deriving (Show, Eq, Generic)

instance NFData LanguageStats where
    rnf (LanguageStats f l b c cd) = rnf f `seq` rnf l `seq` rnf b `seq` rnf c `seq` rnf cd

instance Semigroup LanguageStats where
    (LanguageStats f1 l1 b1 c1 cd1) <> (LanguageStats f2 l2 b2 c2 cd2) = 
        let !f = f1 + f2
            !l = l1 + l2
            !b = b1 + b2
            !c = c1 + c2
            !cd = cd1 + cd2
        in LanguageStats f l b c cd

data BlockCommentStyle = BlockCommentStyle {
    blockStart :: !ByteString,
    blockEnd :: !ByteString
} deriving (Show, Eq, Generic)

instance NFData BlockCommentStyle where
    rnf (BlockCommentStyle bs be) = rnf bs `seq` rnf be

data Language = Language {
    name :: !ByteString,
    extensions :: ![ByteString],
    lineComments :: ![ByteString],
    multiLineComments :: ![BlockCommentStyle]
} deriving (Show, Eq, Generic)

instance NFData Language where
    rnf (Language n exts lc mlc) = rnf n `seq` rnf exts `seq` rnf lc `seq` rnf mlc

type ActiveBlockComments = [BlockCommentStyle]

data AggratedStats = AggratedStats {
    byLanguage :: !(MergeMap ByteString LanguageStats),
    totalStats :: !FileStats
} deriving (Show, Generic)

instance Semigroup AggratedStats where
  (AggratedStats lang1 total1) <> (AggratedStats lang2 total2) = 
      let !lang = lang1 <> lang2
          !total = total1 <> total2
      in AggratedStats lang total

instance NFData AggratedStats where
    rnf (AggratedStats lang total) = rnf lang `seq` rnf total

instance Monoid AggratedStats where
    mempty = AggratedStats mempty mempty
    mappend = (<>)
