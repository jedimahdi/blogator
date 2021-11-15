module Blogator.Run
    ( run
    ) where

import           Blogator.Anatomy.Index
import           Blogator.Anatomy.OutputDirectory
import           Blogator.Anatomy.Posts
import           Blogator.Data.Env
import           Blogator.Effect.FileSystem       ( FileSystem, runFileSystemIO )
import           Data.Text                        ( Text )
import qualified Data.Text                        as T
import           Polysemy                         ( Members, Sem, runM )
import           Polysemy.Error                   ( Error, runError )
import qualified Polysemy.Error                   as Error
import           Polysemy.Reader                  ( Reader, runReader )
import           System.FilePath


program :: Members '[FileSystem, Reader Env, Error Text] r => Sem r ()
program = do
  posts <- readPostFiles
  createOutputDirectory
  createIndex posts
  writePosts posts

env :: Env
env = Env { outputDirectory = "output", postsDirectory = "_site" </> "_posts" }

run :: IO ()
run = do
  runM . runError . runReader env . runFileSystemIO $ program
  pure ()
