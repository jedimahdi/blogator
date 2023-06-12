module Blogator.Data.Date (
  Date,
  mkDate,
  toDay,
) where

import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar

newtype Date
  = Date Day
  deriving newtype (Eq, Ord)

toDay :: Date -> Day
toDay (Date day) = day

mkDate :: Text -> Either Text Date
mkDate t =
  maybe
    (Left "Invalid format for date. expecting format: YYYY-MM-DD")
    Right
    $ case Text.split (== '-') t of
      [yyyy, mm, dd] ->
        fmap Date $
          join $
            fromGregorianValid
              <$> readMaybe yyyy
              <*> readMaybe mm
              <*> readMaybe dd
      _ ->
        Nothing

readMaybe :: Read a => Text -> Maybe a
readMaybe t =
  case reads (Text.unpack t) of
    [(r, [])] -> pure r
    _ -> Nothing
