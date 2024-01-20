module IntSimpleMulti (IntSimpleMulti, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import Digit (Digit)
import qualified Digit
import NonZeroDigit (NonZeroDigit)
import qualified NonZeroDigit

data IntSimpleMulti
  = IntSimpleMulti NonZeroDigit Digit

parse :: Parser IntSimpleMulti
parse =
  do
    first <- NonZeroDigit.parse
    second <- Digit.parse
    pure (IntSimpleMulti first second)

write :: IntSimpleMulti -> ByteString
write (IntSimpleMulti first second) =
  NonZeroDigit.write first <> Digit.write second
