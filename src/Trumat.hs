module Trumat (format) where

import qualified Ast
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

format :: ByteString -> Either String ByteString
format unformatted =
  case Data.Attoparsec.ByteString.Char8.parseOnly Ast.parse unformatted of
    Left err ->
      Left err
    Right ast ->
      Right $ Ast.write ast
