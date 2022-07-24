module Blogator.Run
    ( run
    ) where

import           Blogator.Anatomy.Index
import           Blogator.Anatomy.OutputDirectory
import           Blogator.Anatomy.Posts
import           Blogator.Data.Env
import           Blogator.Effect.FileSystem       ( FileSystem, runFileSystemIO )
import           Blogator.Options
import           Data.Text                        ( Text )
import qualified Data.Text                        as T
import           Polysemy                         ( Members, Sem, runM, Embed )
import           Polysemy.Error                   ( Error, runError )
import qualified Polysemy.Error                   as Error
import           Polysemy.Reader                  ( Reader, runReader )
import           System.Exit
import           System.FilePath

program :: Members '[FileSystem, Reader Env, Error Text, Embed IO] r => Sem r ()
program = do
  posts <- readPostFiles
  createOutputDirectory
  createIndex posts
  writePosts posts

run :: IO ()
run = do
  Options {outputDirectory} <- getOptions
  let env = Env {outputDirectory, postsDirectory = "_site" </> "_posts"}
  s <- runM . runError . runReader env . runFileSystemIO $ program
  case s of
    Left x  -> die (T.unpack x)
    Right _ -> pure ()
