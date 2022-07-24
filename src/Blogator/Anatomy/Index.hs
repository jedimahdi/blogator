module Blogator.Anatomy.Index where


import           Blogator.Data.Env
import           Blogator.Data.Post
import           Blogator.Effect.FileSystem    ( FileSystem )
import qualified Blogator.Effect.FileSystem    as FileSystem
import           Blogator.Html
import           Data.List                     ( sortOn )
import qualified Data.Ord
import           Data.Text                     ( Text )
import qualified Data.Text.Lazy                as TL
import           Polysemy
import           Polysemy.Error
import qualified Polysemy.Error                as Error
import           Polysemy.Reader
import           System.FilePath
import           Text.Blaze.Html.Renderer.Text

createIndex :: Members '[FileSystem, Reader Env] r => [Post] -> Sem r ()
createIndex posts = do
  Env {outputDirectory} <- ask
  let index = template "jedimahdi's Blog" "./" defaultConfig (indexHtml posts)
  FileSystem.writeFile (outputDirectory </> "index.html") (TL.toStrict $ renderHtml index)
