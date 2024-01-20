module HexDigit (HexDigit, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data HexDigit
  = H0

write :: HexDigit -> ByteString
write digit =
  case digit of
    H0 ->
      "0"

parse :: Parser HexDigit
parse =
  do
    char <- Data.Attoparsec.ByteString.Char8.anyChar
    case charToHexDigit char of
      Nothing ->
        fail "expecting a hex digit"
      Just digit ->
        pure digit

charToHexDigit :: Char -> Maybe HexDigit
charToHexDigit char =
  case char of
    '0' ->
      Just H0
    _ ->
      Nothing
