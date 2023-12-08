module Trumat (format) where

import Data.Text (Text)

format :: Text -> Either String Text
format input =
  Right input
