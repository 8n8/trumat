module IntHex (IntHex, write, parse, isZero) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import HexDigit (HexDigit)
import qualified HexDigit

data IntHex
  = IntHex HexDigit [HexDigit]

isZero :: IntHex -> Bool
isZero (IntHex first subsequent) =
  not (any (not . HexDigit.isZero) (first : subsequent))

parse :: Parser IntHex
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.string "0x"
    first <- HexDigit.parse
    subsequent <- Data.Attoparsec.ByteString.Char8.many' HexDigit.parse
    pure (IntHex first subsequent)

write :: IntHex -> ByteString
write (IntHex first subsequent) =
  [ "0x",
    if length subsequent `mod` 2 == 0
      then "0"
      else "",
    HexDigit.write first,
    mconcat (map HexDigit.write subsequent)
  ]
    & mconcat
