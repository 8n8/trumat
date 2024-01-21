module NormalString (NormalString, write, parse) where

import qualified Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)

data NormalString
  = Empty

parse :: Parser NormalString
parse =
  do
  _ <- Data.Attoparsec.ByteString.Char8.string "\"\""
  pure Empty
  
write :: NormalString -> ByteString
write _ =
  "\"\""
