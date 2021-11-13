module Web.Blogator.Run where

import Control.Monad
import Data.List (sortOn)
import qualified Data.ByteString.Lazy as BSL (writeFile)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import System.Directory
import System.FilePath
import Shelly (cp_r, fromText, shelly)
import Text.Blaze.Html.Renderer.Text

import Web.Blogator.Parse (readSite)
import Web.Blogator.Types
import Web.Blogator.Html


run :: IO ()
run = do
  site <- readSite
  mkOutputDir True
  let index = template defaultConfig (map postHtml (reverse . sortOn postDate $ sitePosts site))
  TIO.writeFile ("output" </> "index.html") (renderHtml index)

  forM_ (sitePosts site) $ \p -> do
    let pHtml = template defaultConfig [postHtml p]
    TIO.writeFile ("output" </> (T.unpack (postRoute p) ++ ".html")) (renderHtml pHtml)

mkOutputDir :: Bool -> IO ()
mkOutputDir override = do
  when override $ do
    doesDirectoryExist "output" >>=
      (`when` removeDirectoryRecursive "output")
  
  createDirectory "output"
  shelly $ cp_r (fromText $ T.pack ("_site" </> "static" </> ".")) (fromText "output")
