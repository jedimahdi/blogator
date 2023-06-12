module Blogator.Anatomy.OutputDirectory where

import Blogator.ColorScheme (onedarker)
import Blogator.Data.Env
import Blogator.Data.Post (Post, parsePost)
import Blogator.Effect.FileSystem (FileSystem)
import qualified Blogator.Effect.FileSystem as FileSystem
import Control.Monad (unless, when)
import qualified Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Polysemy
import Polysemy.Error
import qualified Polysemy.Error as Error
import Polysemy.Reader
import Shelly (cp_r, fromText, shelly)
import Skylighting (styleToCss)
import System.FilePath

createOutputDirectory :: Members '[FileSystem, Reader Env] r => Sem r ()
createOutputDirectory = do
  Env{outputDirectory} <- ask
  FileSystem.doesDirectoryExist outputDirectory
    >>= (`when` FileSystem.removeDirectoryRecursive outputDirectory)

  FileSystem.createDirectory outputDirectory
  FileSystem.createDirectory $ outputDirectory </> "posts"
  FileSystem.copyRecursive "_site/static/." outputDirectory

  doesCssExists <- FileSystem.doesDirectoryExist (outputDirectory </> "css")
  unless doesCssExists $ FileSystem.createDirectory (outputDirectory </> "css")
  FileSystem.writeFile (outputDirectory </> "css" </> "syntax.css") (styleToCss onedarker)
