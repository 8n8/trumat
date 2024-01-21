module Digit (Digit, write, parse, isZero) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data Digit
  = D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  deriving (Eq)

isZero :: Digit -> Bool
isZero digit =
  digit == D0

write :: Digit -> ByteString
write digit =
  case digit of
    D0 ->
      "0"
    D1 ->
      "1"
    D2 ->
      "2"
    D3 ->
      "3"
    D4 ->
      "4"
    D5 ->
      "5"
    D6 ->
      "6"
    D7 ->
      "7"
    D8 ->
      "8"
    D9 ->
      "9"

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
    '3' ->
      Just D3
    '4' ->
      Just D4
    '5' ->
      Just D5
    '6' ->
      Just D6
    '7' ->
      Just D7
    '8' ->
      Just D8
    '9' ->
      Just D9
    _ ->
      Nothing
