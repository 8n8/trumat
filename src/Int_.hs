module Int_ (Int_, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import IntHex (IntHex)
import qualified IntHex
import IntSimpleMulti (IntSimpleMulti)
import qualified IntSimpleMulti
import IntSingle (IntSingle)
import qualified IntSingle

data Int_
  = Single IntSingle
  | SimpleMulti IntSimpleMulti
  | Hex IntHex

parse :: Parser Int_
parse =
  [ fmap Hex IntHex.parse,
    fmap SimpleMulti IntSimpleMulti.parse,
    fmap Single IntSingle.parse
  ]
    & Data.Attoparsec.ByteString.Char8.choice

write :: Int_ -> ByteString
write int =
  case int of
    Single i ->
      IntSingle.write i
    SimpleMulti i ->
      IntSimpleMulti.write i
    Hex i ->
      IntHex.write i
