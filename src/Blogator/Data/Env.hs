module Blogator.Data.Env (
  Env (..),
) where

data Env = Env
  { postsDirectory :: FilePath
  , outputDirectory :: FilePath
  }
