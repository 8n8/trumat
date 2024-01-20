module Float_ (Float_, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import Digit (Digit)
import qualified Digit

data Float_
  = Float_ Digit [Digit] Digit [Digit]

write :: Float_ -> ByteString
write (Float_ before1 before2s after1 after2s) =
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

parse :: Parser Float_
parse =
  do
    (before1, before2s) <- someDigits
    _ <- Data.Attoparsec.ByteString.Char8.char '.'
    (after1, after2s) <- someDigits
    pure (Float_ before1 before2s after1 after2s)
