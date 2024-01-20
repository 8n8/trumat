module Bind (Bind, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Expression (Expression)
import qualified Expression

data Bind
  = Bind Expression

parse :: Parser Bind
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.string "x =\n"
    _ <- Data.Attoparsec.ByteString.Char8.takeWhile (\ch -> ch == ' ')
    ch <- Expression.parse
    pure $ Bind ch

write :: Bind -> ByteString
write (Bind expression) =
  "x =\n    " <> Expression.write expression
