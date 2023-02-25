module Trumat (trumat) where

import Text.Megaparsec (errorBundlePretty, parse, Parsec, chunk)
import Data.Void (Void)
import Data.Text (Text)
import qualified Token

trumat :: Text -> Either String Text
trumat raw =
  case Text.Megaparsec.parse Trumat.parse "" raw of
    Left err ->
        Left $ errorBundlePretty err

    Right formatted ->
        Right formatted

type Parser
    = Parsec Void Text

parsePreamble :: Parser ()
parsePreamble =
   do
   _ <- chunk "module X exposing (x)\n\n\nx =\n    "
   return ()

parse :: Parser Text
parse =
    do
    _ <- parsePreamble
    return ""
