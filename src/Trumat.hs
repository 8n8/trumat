module Trumat (trumat) where

import Data.Text (Text, intercalate)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    errorBundlePretty,
    lookAhead,
    many,
    parse,
    takeWhile1P,
    try,
  )
import Text.Megaparsec.Char (char, space)
import Prelude
  ( Either (..),
    Maybe (..),
    String,
    elem,
    mconcat,
    null,
    return,
    ($),
    (<>),
  )

preamble :: Text
preamble =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    "

type Parser = Parsec Void Text

trumat :: Text -> Either String Text
trumat unformatted =
  case parse parser "" unformatted of
    Left err ->
      Left $ errorBundlePretty err
    Right formatted ->
      Right formatted

parser :: Parser Text
parser =
  do
    _ <- chunk preamble
    expression <- parseExpression
    return $ preamble <> expression <> "\n"

parseExpression :: Parser Text
parseExpression =
  choice [parseVerbatim, parseList]

parseVerbatim :: Parser Text
parseVerbatim =
  takeWhile1P
    (Just "verbatim character")
    (\ch -> ch `elem` ("0123456789" :: String))

parseListItem :: Parser Text
parseListItem =
  do
    expression <- parseExpression
    _ <- space
    _ <- choice [char ',', lookAhead (try (char ']'))]
    _ <- space
    return expression

parseList :: Parser Text
parseList =
  do
    _ <- char '['
    _ <- space
    items <- many parseListItem
    _ <- char ']'
    if null items
      then return "[]"
      else
        return $
          mconcat
            [ "[ ",
              intercalate ", " items,
              " ]"
            ]
