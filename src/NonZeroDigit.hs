module NonZeroDigit (NonZeroDigit, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data NonZeroDigit
  = D1

parse :: Parser NonZeroDigit
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.char '1'
    return D1

write :: NonZeroDigit -> ByteString
write digit =
  case digit of
    D1 ->
      "1"
