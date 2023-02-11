module Trumat (trumat) where

import Data.Text (Text)
import Prelude (Either (..), String)

trumat :: Text -> Either String Text
trumat x = Right x
