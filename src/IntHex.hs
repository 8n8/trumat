module IntHex (IntHex, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import HexDigit (HexDigit)
import qualified HexDigit

data IntHex
  = IntHex HexDigit [HexDigit]

parse :: Parser IntHex
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.string "0x"
    first <- HexDigit.parse
    subsequent <- Data.Attoparsec.ByteString.Char8.many' HexDigit.parse
    pure (IntHex first subsequent)

write :: IntHex -> ByteString
write (IntHex first subsequent) =
  "0x" <> HexDigit.write first <> mconcat (map HexDigit.write subsequent)
