module Bind (Bind, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString
import qualified Data.Char
import Digit (Digit)
import qualified Digit

data Bind
  = Bind Digit

parse :: Parser Bind
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.string "x =\n"
    _ <- Data.Attoparsec.ByteString.Char8.takeWhile (\ch -> ch == ' ')
    ch <- Digit.parse
    pure $ Bind ch

write :: Bind -> ByteString
write (Bind ch) =
  "x =\n    " <> Digit.write ch
