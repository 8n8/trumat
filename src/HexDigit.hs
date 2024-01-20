module HexDigit (HexDigit, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data HexDigit
  = H0
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | H7
  | H8
  | H9
  | A
  | B
  | C
  | D
  | E
  | F

write :: HexDigit -> ByteString
write digit =
  case digit of
    H0 ->
      "0"
    H1 ->
      "1"
    H2 ->
      "2"
    H3 ->
      "3"
    H4 ->
      "4"
    H5 ->
      "5"
    H6 ->
      "6"
    H7 ->
      "7"
    H8 ->
      "8"
    H9 ->
      "9"
    A ->
      "A"
    B ->
      "B"
    C ->
      "C"
    D ->
      "D"
    E ->
      "E"
    F ->
      "F"

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
    '1' ->
      Just H1
    '2' ->
      Just H2
    '3' ->
      Just H3
    '4' ->
      Just H4
    '5' ->
      Just H5
    '6' ->
      Just H6
    '7' ->
      Just H7
    '8' ->
      Just H8
    '9' ->
      Just H9
    'A' ->
      Just A
    'B' ->
      Just B
    'C' ->
      Just C
    'D' ->
      Just D
    'E' ->
      Just E
    'F' ->
      Just F
    'a' ->
      Just A
    _ ->
      Nothing
