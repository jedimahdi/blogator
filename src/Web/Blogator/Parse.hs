module Web.Blogator.Parse where

import Cheapskate
import Control.Arrow
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Data.Time.Calendar
import System.Directory (listDirectory)
import System.Exit
import System.FilePath
import Web.Blogator.Types
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Highlighting.Kate

readSite :: IO Site
readSite =
  Site
    <$> posts

side :: IO Html
side = do
  navFile <- TIO.readFile ("_site" </> "side.md")
  pure $ toHtml $ markdown markdownOptions navFile

posts :: IO [Post]
posts = do
  dir <- listDirectory ("_site" </> "_posts")
  postsTexts <- zip dir <$> traverse (\f -> TIO.readFile $ "_site" </> "_posts" </> f) dir
  traverse
    (uncurry readPost)
    postsTexts

readPost :: FilePath -> Text -> IO Post
readPost f =
  do
    either
      (\e -> die ("Error in file: " <> f <> ": " <> T.unpack e))
      pure
    . parsePost

addHighlighting :: Block -> Block
addHighlighting (CodeBlock (CodeAttr lang _) t) =
  HtmlBlock (T.concat $ TL.toChunks
             $ renderHtml $ toHtml
             $ formatHtmlBlock defaultFormatOpts
             $ highlightAs (T.unpack lang) (T.unpack t))
addHighlighting x = x

parsePost :: Text -> Either Text Post
parsePost =
  (\(m, t) -> ($ toHtml $ walk addHighlighting $ markdown markdownOptions t) <$> parseMeta m)
    . second (T.unlines . drop 1)
    . break (== "---")
    . T.lines
  where
    parseMeta meta =
      Post
        <$> match "route" lt
        <*> match "title" lt
        <*> (parseDay =<< match "date" lt)
      where
        lt = map ((T.toLower *** T.drop 1) . T.break (== '=')) meta

match :: Text -> [(Text, a)] -> Either Text a
match key =
  maybe
    (Left $ "Could not find the metadata field: '" <> key <> "'.")
    Right
    . lookup key

parseDay :: Text -> Either Text Day
parseDay t =
  maybe
    (Left "Invalid format for date. expecting format: YYYY-MM-DD")
    Right
    $ case T.split (== '-') t of
      [yyyy, mm, dd] ->
        join $
          fromGregorianValid
            <$> readMaybe yyyy
            <*> readMaybe mm
            <*> readMaybe dd
      _ ->
        Nothing

readMaybe :: Read a => Text -> Maybe a
readMaybe t =
  case reads (T.unpack t) of
    [(r, [])] -> pure r
    _ -> Nothing