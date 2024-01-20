module Bind (Bind, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Int_ (Int_)
import qualified Int_

data Bind
  = Bind Int_

parse :: Parser Bind
parse =
  do
    _ <- Data.Attoparsec.ByteString.Char8.string "x =\n"
    _ <- Data.Attoparsec.ByteString.Char8.takeWhile (\ch -> ch == ' ')
    ch <- Int_.parse
    pure $ Bind ch

write :: Bind -> ByteString
write (Bind ch) =
  "x =\n    " <> Int_.write ch
