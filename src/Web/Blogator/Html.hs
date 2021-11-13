{-# LANGUAGE OverloadedStrings #-}

module Web.Blogator.Html where

import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Text.Blaze.Html (Html, textValue)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Highlighting.Kate (styleToCss, espresso)
import Web.Blogator.Types
import Data.Coerce (coerce)

data Config = Config
  { cfgTitle :: T.Text,
    cfgDesc :: T.Text,
    cfgLang :: T.Text,
    cfgRtl :: Bool
  }
  deriving (Show, Eq, Ord)

defaultConfig :: Config
defaultConfig =
  Config
    { cfgTitle = "Example",
      cfgDesc = "A hen example website",
      cfgLang = "en",
      cfgRtl = False
    }

template :: Config -> [Html] -> Html
template cfg posts = do
  H.docType
  H.html ! A.lang (textValue $ cfgLang cfg) $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title $ toHtml (cfgTitle cfg) 

      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "css/style.css"

      H.style ! A.type_ "text/css" $ toHtml $ styleToCss espresso
    
    H.body $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.id "title" $ do
          H.h1 $ H.a ! A.href "index.html" $ toHtml (cfgTitle cfg)
          H.p $ toHtml (cfgDesc cfg)

        H.div ! A.class_ "wrapper" $ do
          H.div ! A.class_ "content" $ do
            mapM_ (H.div ! A.class_ "post") posts

postHtml :: Post -> Html
postHtml Post {..} = do
  H.h2 $ H.a ! A.href (textValue postRoute <> ".html") $ toHtml postTitle
  H.h5 $ date postDate
  postBody
  where
    date (toGregorian -> (yyyy, mm, dd)) =
      toHtml $ show dd <> "/" <> show mm <> "/" <> show yyyy
