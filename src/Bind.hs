module Bind (Bind, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString
import qualified Data.Char

data Bind
  = Bind Char

parse :: Parser Bind
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.string "x =\n"
    _ <- Data.Attoparsec.ByteString.Char8.takeWhile (\ch -> ch == ' ')
    ch <- Data.Attoparsec.ByteString.Char8.anyChar
    pure $ Bind ch

write :: Bind -> ByteString
write (Bind ch) =
  "x =\n    " <> Data.ByteString.singleton (fromIntegral (Data.Char.ord ch))
