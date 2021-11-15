module Blogator.Anatomy.OutputDirectory
    where


import           Blogator.Data.Env
import           Blogator.Data.Post         ( Post, parsePost )
import           Blogator.Effect.FileSystem ( FileSystem )
import qualified Blogator.Effect.FileSystem as FileSystem
import           Control.Monad              ( when )
import qualified Data.Ord
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text
import           Polysemy
import           Polysemy.Error
import qualified Polysemy.Error             as Error
import           Polysemy.Reader
import           System.FilePath

createOutputDirectory :: Members '[FileSystem, Reader Env] r => Sem r ()
createOutputDirectory = do
  Env {outputDirectory} <- ask
  FileSystem.doesDirectoryExist "output" >>=
    (`when` FileSystem.removeDirectoryRecursive "output")

  FileSystem.createDirectory "output"
  -- shelly $ cp_r (fromText $ Text.pack ("_site" </> "static" </> ".")) (fromText "output")
