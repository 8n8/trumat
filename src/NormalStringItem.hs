module NormalStringItem (NormalStringItem, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data NormalStringItem
  = Aa

parse :: Parser NormalStringItem
parse =
  Data.Attoparsec.ByteString.Char8.char 'a' >> pure Aa

write :: NormalStringItem -> ByteString
write _ =
  "a"
