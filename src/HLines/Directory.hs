module HLines.Directory where

import Conduit
import Control.Monad (guard)
import Data.List (all)
import Data.Maybe
import qualified Data.Streaming.Filesystem as F
import System.FilePath.GlobPattern
import System.IO

{-
  default exclude all files under ".**/" node_modules vendor
-}
defaultExcludeDirs :: [GlobPattern]
defaultExcludeDirs = [".*", "**/.*", "**/.*/**", "node_modules", "**/node_modules", "**/node_modules/**", "vendor", "**/vendor", "**/vendor/**"]

noExcludeDir, includeDir :: [GlobPattern] -> FilePath -> Bool
noExcludeDir patterns fp = all (\pattern -> fp /~ pattern) patterns'
  where
    patterns' = concat [defaultExcludeDirs, patterns]

includeDir patterns fp = all (\pattern -> fp ~~ pattern) patterns

sourceDirectoryDeep' :: MonadResource m => (FilePath -> Bool) -> FilePath -> ConduitT i FilePath m ()
sourceDirectoryDeep' matchDir = start
  where
    start :: MonadResource m => FilePath -> ConduitT i FilePath m ()
    start dir = sourceDirectory dir .| awaitForever go
    go :: MonadResource m => FilePath -> ConduitT i FilePath m ()
    go fp = do
      -- liftIO $ print fp
      ft <- liftIO $ F.getFileType fp
      case ft of
        F.FTFile -> yield fp
        F.FTDirectory ->
          if matchDir fp
            then start fp
            else return ()
        _ -> return ()

sourceDir :: MonadIO m => Maybe [GlobPattern] -> Maybe [GlobPattern] -> ConduitT FilePath FilePath (ResourceT m) ()
sourceDir includeP excludeP = awaitForever (\fp -> sourceDirectoryDeep' match fp)
  where
    matchInclude =
      case includeP of
        Nothing -> const True
        Just p -> includeDir p
    noMatchExclude = noExcludeDir $ fromMaybe ([] :: [FilePath]) excludeP
    match = \fp -> matchInclude fp && noMatchExclude fp
