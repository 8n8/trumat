module Float_ (Float_, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import FloatExponent (FloatExponent)
import qualified FloatExponent
import FloatSimple (FloatSimple)
import qualified FloatSimple

data Float_
  = Float_ Sign Positive

data Sign
  = Plus
  | Minus

parseSign :: Parser Sign
parseSign =
  [ Data.Attoparsec.ByteString.Char8.char '-' >> pure Minus,
    pure Plus
  ]
    & Data.Attoparsec.ByteString.Char8.choice

data Positive
  = Simple FloatSimple
  | Exponent FloatExponent

write :: Float_ -> ByteString
write (Float_ sign positive) =
  writeSign sign <> writePositive positive

writeSign :: Sign -> ByteString
writeSign sign =
  case sign of
    Plus ->
      ""
    Minus ->
      "-"

writePositive :: Positive -> ByteString
writePositive float =
  case float of
    Simple f ->
      FloatSimple.write f
    Exponent f ->
      FloatExponent.write f

parse :: Parser Float_
parse =
  do
    sign <- parseSign
    positive <- parsePositive
    pure (Float_ sign positive)

parsePositive :: Parser Positive
parsePositive =
  [ fmap Exponent FloatExponent.parse,
    fmap Simple FloatSimple.parse
  ]
    & Data.Attoparsec.ByteString.Char8.choice
