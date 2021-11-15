module Blogator.Data.Post
    ( Post (..)
    , parsePost
    ) where

import           Blogator.Data.Date            ( Date, mkDate )
import           Cheapskate
import           Control.Arrow
import           Data.Text                     ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as TL
import           GHC.Generics                  ( Generic )
import           Text.Blaze.Html               ( Html, toHtml )
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Text.Highlighting.Kate

data Post = Post { route :: Text
                 , title :: Text
                 , date  :: Date
                 , body  :: Html
                 }


addHighlighting :: Block -> Block
addHighlighting (CodeBlock (CodeAttr lang _) t) =
  HtmlBlock (Text.concat $ TL.toChunks
             $ renderHtml $ toHtml
             $ formatHtmlBlock defaultFormatOpts
             $ highlightAs (Text.unpack lang) (Text.unpack t))
addHighlighting x = x

markdownOptions :: Options
markdownOptions =
  Options
    { sanitize = True
    , allowRawHtml = True
    , preserveHardBreaks = True
    , debug = False
    }

parsePost :: Text -> Either Text Post
parsePost =
  (\(m, t) -> ($ toHtml $ walk addHighlighting $ markdown markdownOptions t) <$> parseMeta m)
    . second (Text.unlines . drop 1)
    . break (== "---")
    . Text.lines
  where
    parseMeta meta =
      Post
        <$> match "route" lt
        <*> match "title" lt
        <*> (mkDate =<< match "date" lt)
      where
        lt = map ((Text.toLower *** Text.drop 1) . Text.break (== '=')) meta

match :: Text -> [(Text, a)] -> Either Text a
match key =
  maybe
    (Left $ "Could not find the metadata field: '" <> key <> "'.")
    Right
    . lookup key

