{-# LANGUAGE TemplateHaskell #-}

module Blogator.Effect.FileSystem where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import qualified System.Directory as Directory
import System.Process (callProcess)

data FileSystem m a where
  ListDirectory :: FilePath -> FileSystem m [FilePath]
  CreateDirectory :: FilePath -> FileSystem m ()
  RemoveDirectoryRecursive :: FilePath -> FileSystem m ()
  DoesDirectoryExist :: FilePath -> FileSystem m Bool
  CopyRecursive :: FilePath -> FilePath -> FileSystem m ()
  ReadFile :: FilePath -> FileSystem m Text
  WriteFile :: FilePath -> String -> FileSystem m ()

makeSem ''FileSystem

runFileSystemIO :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret $ \case
  ListDirectory path -> embed $ Directory.listDirectory path
  CreateDirectory path -> embed $ Directory.createDirectory path
  RemoveDirectoryRecursive path -> embed $ Directory.removeDirectoryRecursive path
  DoesDirectoryExist path -> embed $ Directory.doesDirectoryExist path
  CopyRecursive src dest -> embed $ callProcess "cp" ["-r", src, dest]
  ReadFile path -> embed $ TIO.readFile path
  WriteFile path content -> embed $ Prelude.writeFile path content
