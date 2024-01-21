module IntSingle (IntSingle, write, parse, isZero) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import Digit (Digit)
import qualified Digit

newtype IntSingle
  = IntSingle Digit

isZero :: IntSingle -> Bool
isZero (IntSingle digit) =
  Digit.isZero digit

parse :: Parser IntSingle
parse =
  fmap IntSingle Digit.parse

write :: IntSingle -> ByteString
write (IntSingle digit) =
  Digit.write digit
