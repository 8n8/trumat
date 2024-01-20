module IntSimpleMulti (IntSimpleMulti, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Digit (Digit)
import qualified Digit
import NonZeroDigit (NonZeroDigit)
import qualified NonZeroDigit

data IntSimpleMulti
  = IntSimpleMulti NonZeroDigit Digit [Digit]

parse :: Parser IntSimpleMulti
parse =
  do
    first <- NonZeroDigit.parse
    second <- Digit.parse
    subsequent <- Data.Attoparsec.ByteString.Char8.many' Digit.parse
    pure (IntSimpleMulti first second subsequent)

write :: IntSimpleMulti -> ByteString
write (IntSimpleMulti first second subsequent) =
  NonZeroDigit.write first <> Digit.write second <> mconcat (map Digit.write subsequent)
