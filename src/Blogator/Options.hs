module Blogator.Options where

import           Data.Text       ( Text )
import           Options.Generic

data Options = Options { outputDirectory :: FilePath
                       }
  deriving (Generic, Show)

instance ParseRecord Options

getOptions :: IO Options
getOptions = getRecord "Blogator"
