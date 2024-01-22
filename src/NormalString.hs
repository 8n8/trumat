module NormalString (NormalString, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import NormalStringItem (NormalStringItem)
import qualified NormalStringItem

data NormalString
  = NormalString [NormalStringItem]

parse :: Parser NormalString
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.char '"'
    items <- Data.Attoparsec.ByteString.Char8.many' NormalStringItem.parse
    _ <- Data.Attoparsec.ByteString.Char8.char '"'
    pure (NormalString items)

write :: NormalString -> ByteString
write (NormalString items) =
  [ "\"",
    mconcat (map NormalStringItem.write items),
    "\""
  ]
    & mconcat
