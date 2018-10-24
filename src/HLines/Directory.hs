module HLines.Directory where

import System.Directory
import System.FilePath ((</>))

walk :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
walk match filepath = do
  isDir <- doesDirectoryExist filepath
  isFile <- doesFileExist filepath
  if isDir
    then do
      files <- listDirectory filepath
      files' <- mapM (\file -> walk match (filepath </> file)) files
      return $ concat files'
    else if isFile
           then return [filepath]
           else return []

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles = walk $ const True
