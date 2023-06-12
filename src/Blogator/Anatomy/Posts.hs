module Blogator.Anatomy.Posts where

import Blogator.Data.Env
import Blogator.Data.Post
import Blogator.Effect.FileSystem (FileSystem)
import qualified Blogator.Effect.FileSystem as FileSystem
import Blogator.Html
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Polysemy
import Polysemy.Error
import qualified Polysemy.Error as Error
import Polysemy.Reader
import System.FilePath
import Text.Blaze.Html (Html, textValue)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as H hiding (title)
import Text.Blaze.Html5.Attributes as A hiding (title)

metaHeaderPost :: Post -> Html
metaHeaderPost Post{..} = do
  H.meta ! A.name "twitter:title" ! A.content (H.textValue title)
  -- H.meta ! A.name "twitter:description" ! A.content (H.textValue title)
  H.meta ! A.name "twitter:creator" ! A.content "@jedimahdi"
  H.meta ! A.name "twitter:site" ! A.content "@jedimahdi"

-- H.meta ! A.name "twitter:image" ! A.content "@jedimahdi"

writePosts :: Members '[FileSystem, Reader Env] r => [Post] -> Sem r ()
writePosts posts = do
  Env{outputDirectory} <- ask
  forM_ posts $ \p -> do
    let pHtml = template (title p) "../" defaultConfig (metaHeaderPost p) (postHtml p)
    FileSystem.writeFile (outputDirectory </> "posts" </> (Text.unpack (route p) ++ ".html")) (renderHtml pHtml)

readPostFiles :: Members '[FileSystem, Reader Env, Error Text] r => Sem r [Post]
readPostFiles = do
  Env{postsDirectory} <- ask
  postsFiles <- FileSystem.listDirectory postsDirectory
  traverse readPostFile postsFiles

readPostFile :: Members '[FileSystem, Reader Env, Error Text] r => FilePath -> Sem r Post
readPostFile filename = do
  Env{postsDirectory} <- ask
  content <- FileSystem.readFile $ postsDirectory </> filename
  either throw pure $ parsePost content
