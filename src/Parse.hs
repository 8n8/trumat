module Parse (parse) where

import Data.Attoparsec.ByteString (Parser, takeByteString)
import Data.ByteString (ByteString)

parse :: Parser ByteString
parse =
  takeByteString
