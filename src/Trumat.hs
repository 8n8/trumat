module Trumat (trumat) where

import Data.Text (Text)

trumat :: Text -> Either String Text
trumat unformatted =
  Right unformatted
