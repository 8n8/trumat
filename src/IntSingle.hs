module IntSingle (IntSingle, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import Digit (Digit)
import qualified Digit

newtype IntSingle
  = IntSingle Digit

parse :: Parser IntSingle
parse =
  fmap IntSingle Digit.parse

write :: IntSingle -> ByteString
write (IntSingle digit) =
  Digit.write digit
