module NormalStringItem (NormalStringItem, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))

data NormalStringItem
  = Aa
  | Bb
  | Cc
  | Space

parse :: Parser NormalStringItem
parse =
  [ Data.Attoparsec.ByteString.Char8.char 'a' >> pure Aa,
    Data.Attoparsec.ByteString.Char8.char 'b' >> pure Bb,
    Data.Attoparsec.ByteString.Char8.char 'c' >> pure Cc,
    Data.Attoparsec.ByteString.Char8.char ' ' >> pure Space
  ]
    & Data.Attoparsec.ByteString.Char8.choice

write :: NormalStringItem -> ByteString
write item =
  case item of
    Aa ->
      "a"
    Bb ->
      "b"
    Cc ->
      "c"
    Space ->
      " "
