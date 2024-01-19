module Digit (Digit, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data Digit
  = D0
  | D1
  | D2

write :: Digit -> ByteString
write digit =
  case digit of
    D0 ->
      "0"
    D1 ->
      "1"
    D2 ->
      "2"

parse :: Parser Digit
parse =
  do
    ch <- Data.Attoparsec.ByteString.Char8.anyChar
    case charToDigit ch of
      Nothing ->
        fail "expecting a digit character"
      Just digit ->
        pure digit

charToDigit :: Char -> Maybe Digit
charToDigit ch =
  case ch of
    '0' ->
      Just D0
    '1' ->
      Just D1
    '2' ->
      Just D2
    _ ->
      Nothing
