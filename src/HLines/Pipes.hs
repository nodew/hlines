{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module HLines.Pipes where

import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Exception (catch)
import Control.Monad
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
import Data.IORef

sourceFiles :: FilePath -> Producer' FilePath IO ()
sourceFiles path = do
  files <- liftIO $ listDirectory path
  each files >-> P.takeWhile (/= "") >-> P.map (path </>) >->
    forever
      (do entry <- await
          isDir <- liftIO $ doesDirectoryExist entry
          if isDir
            then sourceFiles entry
            else yield entry)

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
      runEffect $ fromInput input' >-> for cat (\(lang, count) -> liftIO $ modifyIORef' m (flip mergeByLang (lang, count)))
      performGC
  as <-
    forM [1 .. 3] $ \_ ->
      async $ do
        runEffect $ fromInput input >-> readContent >-> counter >-> toOutput output'
        performGC
  a <-
    async $ do
      runEffect $ mapM_ sourceFiles files >-> toOutput output
      performGC
  
  mapM_ wait (a' : a : as)
  m' <- readIORef m
  print m'
