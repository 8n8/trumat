module Trumat (trumat) where

import Data.Text
import Tokens
import Text.Megaparsec (errorBundlePretty, parse)
import Ast

trumat :: Text -> Either String Text
trumat raw =
  case Text.Megaparsec.parse Tokens.parse "" raw of
    Left err ->
        Left $ errorBundlePretty err

    Right tokens ->
        case Ast.create tokens of
            Left err ->
                Left err

            Right ast ->
                Rith $ format ast
