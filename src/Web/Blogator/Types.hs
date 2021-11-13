module Web.Blogator.Types where

import Cheapskate (Options(..))
import Data.Text (Text)
import Text.Blaze.Html (Html)
import Data.Time.Calendar

newtype Site = Site
  { sitePosts :: [Post]
  }

data Post = Post
  { postRoute :: Text
  , postTitle :: Text
  , postDate  :: Day
  , postBody  :: Html
  }

markdownOptions :: Options
markdownOptions =
  Options
    { sanitize = True
    , allowRawHtml = True
    , preserveHardBreaks = True
    , debug = False
    }
