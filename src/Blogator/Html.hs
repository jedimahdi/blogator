module Blogator.Html where

import Blogator.ColorScheme (onedarker)
import Blogator.Data.Date (toDay)
import Blogator.Data.Post
import Data.Coerce (coerce)
import Data.List (sortOn)
import qualified Data.Ord
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Calendar (toGregorian)
import Skylighting.Format.HTML (styleToCss)
import Skylighting.Styles (espresso)
import Text.Blaze.Html (Html, textValue)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (attribute)

data Config = Config
  { cfgTitle :: T.Text
  , cfgDesc :: T.Text
  , cfgLang :: T.Text
  , cfgRtl :: Bool
  , cfgStyle :: AttributeValue
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgTitle = "Home"
    , cfgDesc = "A hen example website"
    , cfgLang = "en"
    , cfgRtl = False
    , cfgStyle = "css/style.css"
    }

tagManagerScript :: Html
tagManagerScript = toHtml scriptText
 where
  scriptText :: T.Text
  scriptText = "(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src= 'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f); })(window,document,'script','dataLayer','GTM-WRT3GMF');"

template :: T.Text -> AttributeValue -> Config -> Html -> Html -> Html
template title rootUrl cfg metaOutlet outlet = do
  H.docType
  H.html ! A.lang (textValue $ cfgLang cfg) $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content "The Personal blog of Mahdi Seyedan"
      H.meta ! A.name "author" ! A.content "Mahdi Seyedan"
      H.title $ toHtml title

      metaOutlet

      H.link ! A.rel "icon" ! A.href (rootUrl <> "favicon.ico")
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (rootUrl <> "css/theme.css")
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (rootUrl <> cfgStyle cfg)
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (rootUrl <> "css/syntax.css")

      H.link ! A.rel "preconnect" ! A.href "https://fonts.gstatic.com" ! attribute "crossorigin" " crossorigin=\"" ""
      H.link
        ! A.rel "stylesheet"
        ! A.type_ "text/css"
        ! A.href "https://fonts.googleapis.com/css2?family=JetBrains+Mono&family=Open+Sans&display=swap"
      H.script tagManagerScript

    H.body $ do
      H.div ! A.class_ "container" $ do
        H.header $ do
          H.nav $ do
            H.a ! A.href (rootUrl <> "index.html") $ "Home"
            H.a ! A.href "https://github.com/jedimahdi/" $ "Github"
            H.a ! A.href "https://twitter.com/jedimahdi" $ "Twitter"

        H.div ! A.class_ "wrapper" $ do
          H.div ! A.class_ "content" $ do
            outlet

indexHtml :: [Post] -> Html
indexHtml posts =
  H.section ! A.class_ "table-of-contents" $ do
    H.h1 "All Posts"
    H.ul $
      mapM_ H.li (fmap postSummary (sortOn (Data.Ord.Down . date) posts))

postSummary :: Post -> Html
postSummary Post{..} = do
  H.a ! A.href ("posts/" <> textValue route <> ".html") $ do
    toHtml title
    H.span ! A.class_ "date" $ fromDate date
 where
  fromDate day = toHtml $ formatTime defaultTimeLocale "%b %e, %Y" (toDay day)

postHtml :: Post -> Html
postHtml Post{..} = do
  H.article ! A.class_ "post" $ do
    H.h1 ! A.class_ "title" $ toHtml title
    body
