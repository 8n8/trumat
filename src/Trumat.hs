module Trumat (format) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (chunk, takeWhileP)
import qualified Text.Megaparsec as Megaparsec

format :: Text -> Either String Text
format unformatted =
  case Megaparsec.parse parser "" unformatted of
    Left err ->
      Left (Megaparsec.errorBundlePretty err)
    Right formatted ->
      Right formatted

type Parser =
  Megaparsec.Parsec Void Text

parser :: Parser Text
parser =
  do
    _ <- chunk "module"
    _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    name <- takeWhileP Nothing (\ch -> ch /= ' ')
    remainder <- takeWhileP Nothing (\_ -> True)
    return $ "module " <> name <> remainder
