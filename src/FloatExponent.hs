module FloatExponent (FloatExponent, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import Digit (Digit)
import qualified Digit

parseSign :: Parser Sign
parseSign =
  [ Data.Attoparsec.ByteString.Char8.char '-' >> pure Minus,
    pure Plus
  ]
    & Data.Attoparsec.ByteString.Char8.choice

parseAfterDot :: Parser [Digit]
parseAfterDot =
  [ do
      _ <- Data.Attoparsec.ByteString.Char8.char '.'
      Data.Attoparsec.ByteString.Char8.many1' Digit.parse,
    pure []
  ]
    & Data.Attoparsec.ByteString.Char8.choice

data Sign
  = Plus
  | Minus

writeSign :: Sign -> ByteString
writeSign sign =
  case sign of
    Plus ->
      ""
    Minus ->
      "-"

data FloatExponent
  = FloatExponent Digit [Digit] [Digit] Sign Digit [Digit]

parse :: Parser FloatExponent
parse =
  do
    before1 <- Digit.parse
    before2s <- Data.Attoparsec.ByteString.Char8.many' Digit.parse
    afterDot <- parseAfterDot
    _ <- Data.Attoparsec.ByteString.Char8.char 'e'
    sign <- parseSign
    exponent1 <- Digit.parse
    exponent2s <- Data.Attoparsec.ByteString.Char8.many' Digit.parse
    pure (FloatExponent before1 before2s afterDot sign exponent1 exponent2s)

write :: FloatExponent -> ByteString
write (FloatExponent before1 before2s afterDot sign exponent1 exponent2s) =
  [ Digit.write before1,
    mconcat (map Digit.write before2s),
    ".",
    case afterDot of
      [] ->
        "0"
      _ : _ ->
        mconcat (map Digit.write afterDot),
    "e",
    writeSign sign,
    Digit.write exponent1,
    mconcat (map Digit.write exponent2s)
  ]
    & mconcat
