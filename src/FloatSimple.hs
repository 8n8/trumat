module FloatSimple (FloatSimple, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import Digit (Digit)
import qualified Digit

data FloatSimple
  = FloatSimple Digit [Digit] Digit [Digit]

write :: FloatSimple -> ByteString
write (FloatSimple before1 before2s after1 after2s) =
  [ Digit.write before1,
    mconcat (map Digit.write before2s),
    ".",
    Digit.write after1,
    mconcat (map Digit.write after2s)
  ]
    & mconcat

someDigits :: Parser (Digit, [Digit])
someDigits =
  do
    first <- Digit.parse
    subsequent <- Data.Attoparsec.ByteString.Char8.many' Digit.parse
    pure (first, subsequent)

parse :: Parser FloatSimple
parse =
  do
    (before1, before2s) <- someDigits
    _ <- Data.Attoparsec.ByteString.Char8.char '.'
    (after1, after2s) <- someDigits
    pure (FloatSimple before1 before2s after1 after2s)
