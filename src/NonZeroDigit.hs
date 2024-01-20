module NonZeroDigit (NonZeroDigit, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data NonZeroDigit
  = D1
  | D2
  | D3

parse :: Parser NonZeroDigit
parse =
  do
    ch <- Data.Attoparsec.ByteString.Char8.anyChar
    case charToDigit ch of
      Nothing ->
        fail "expecting a non-zero digit"
      Just digit ->
        pure digit

charToDigit :: Char -> Maybe NonZeroDigit
charToDigit ch =
  case ch of
    '1' ->
      Just D1
    '2' ->
      Just D2
    '3' ->
      Just D3
    _ ->
      Nothing

write :: NonZeroDigit -> ByteString
write digit =
  case digit of
    D1 ->
      "1"
    D2 ->
      "2"
    D3 ->
      "3"
