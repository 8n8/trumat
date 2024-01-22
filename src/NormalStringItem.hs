module NormalStringItem (NormalStringItem, parse, write) where

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8

data NormalStringItem
  = Aa


parse :: Parser NormalStringItem
parse =
  Data.Attoparsec.ByteString.Char8.char 'a' >> pure Aa

write :: NormalStringItem -> ByteString
write _ =
  "a"
