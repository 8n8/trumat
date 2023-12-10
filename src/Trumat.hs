module Trumat (format) where

import Data.Text (Text)
import qualified Ast

format :: Text -> Either String Text
format input =
  case Ast.parse input of
    Left err -> Left $ show err
    Right ast ->
      Right $ Ast.toString ast
