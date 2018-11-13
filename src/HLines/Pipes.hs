{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module HLines.Pipes where

import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Exception (catch)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import HLines.Counter
import HLines.Internal
import HLines.Language
import HLines.Type
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as PS
import System.Directory
import System.FilePath
import System.FilePath.GlobPattern

defaultIgnore = [".git", "vendor", "node_modules"]

filterInclude includePattern =
  P.filter $ \fp ->
    if null includePattern
      then True
      else any ((~~) fp) includePattern

filterExclude excludePattern = P.filter $ \fp -> not $ any ((~~) fp) excludePattern

filterDefaultIgnore :: MonadIO m => Pipe FilePath FilePath m ()
filterDefaultIgnore = P.filter $ \fp -> not $ elem fp defaultIgnore

readIgnore :: FilePath -> IO [String]
readIgnore path = do
  content <- liftIO $ readFile (path </> ".gitignore")
  let lines' = lines content
  return $
    map
      (\line ->
         if last line == '/'
           then init line
           else line)
      lines'

sourceFiles :: MonadIO m => FilePath -> Producer' FilePath (ReaderT ([GlobPattern], [GlobPattern]) m) ()
sourceFiles fp = do
  isDir <- liftIO $ doesDirectoryExist fp
  if not isDir
    then yield fp
    else do
      files <- liftIO $ listDirectory fp
      (include, exclude) <- ask
      ignore <-
        if elem ".gitignore" files
          then liftIO $ readIgnore fp
          else return []
      let exclude' = exclude ++ map (fp </>) ignore
      each files >-> filterDefaultIgnore >-> P.map (fp </>) >-> filterInclude include >-> filterExclude exclude' >->
        forever
          (do entry <- await
              local (const (include, exclude')) $ sourceFiles entry)

readContent :: Pipe FilePath (Language, Comment, Lines) IO ()
readContent =
  forever $ do
    fp <- await
    (lang, comment, content) <- lift $ readLines fp
    yield (lang, comment, content)

counter :: Pipe (Language, Comment, Lines) (Language, Count) IO ()
counter =
  forever $ do
    (lang, comment, content) <- await
    let count = countLines comment content
    yield (lang, count)

runP :: Options -> IO ()
runP Options {..} = do
  (output, input) <- spawn unbounded
  (output', input') <- spawn unbounded
  m <- newIORef (Map.empty :: Map.Map Language Count)
  a' <-
    async $ do
      runEffect $
        fromInput input' >-> for cat (\(lang, count) -> liftIO $ modifyIORef' m (flip mergeByLang (lang, count)))
      performGC
  as <-
    forM [1 .. 3] $ \_ ->
      async $ do
        runEffect $ fromInput input >-> readContent >-> counter >-> toOutput output'
        performGC
  a <-
    async $ do
      flip runReaderT ([], []) $ runEffect $ mapM_ sourceFiles files >-> toOutput output
      performGC
  mapM_ wait (a' : a : as)
  m' <- readIORef m
  print m'
