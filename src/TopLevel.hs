module TopLevel (TopLevel, parse, write) where

import Bind (Bind)
import qualified Bind
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)

data TopLevel
  = Bind Bind

parse :: Parser TopLevel
parse =
  fmap Bind Bind.parse

write :: TopLevel -> ByteString
write (Bind bind) =
  Bind.write bind
