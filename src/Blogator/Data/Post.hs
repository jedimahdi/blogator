module Blogator.Data.Post
    ( Post (..)
    , parsePost
    ) where

import           Blogator.Data.Date            ( Date, mkDate )
import           Cheapskate
import           Control.Arrow
import           Data.Text                     ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Lazy                ( toStrict )
import qualified Data.Text.Lazy                as TL
import           GHC.Generics                  ( Generic )
import           Skylighting                   ( SourceLine, Syntax, lookupSyntax, tokenize )
import           Skylighting.Core
    ( TokenizerConfig (..), defaultFormatOpts, formatHtmlBlock, formatHtmlInline )
import           Skylighting.Syntax            ( defaultSyntaxMap )
import           Text.Blaze.Html               ( Html, toHtml )
import           Text.Blaze.Html.Renderer.Text ( renderHtml )

data Post = Post { route :: Text
                 , title :: Text
                 , date  :: Date
                 , body  :: Html
                 }

getSyntax :: Text -> Syntax
getSyntax lang = case lookupSyntax lang defaultSyntaxMap of
                   Nothing -> error "Code with unsupported language."
                   Just s  -> s

highlightAST :: Text -> Text -> [SourceLine]
highlightAST lang code = case tokenize (TokenizerConfig defaultSyntaxMap False) (getSyntax lang) code of
                           Left e  -> error e
                           Right x -> x

addHighlighting :: Block -> Block
addHighlighting (CodeBlock (CodeAttr lang _) t) =
  HtmlBlock ( toStrict $ renderHtml $ toHtml
             $ formatHtmlBlock defaultFormatOpts
             $ highlightAST lang t)
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

