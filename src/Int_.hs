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
  = Int_ Sign Positive

data Positive
  = Single IntSingle
  | SimpleMulti IntSimpleMulti
  | Hex IntHex

isZero :: Positive -> Bool
isZero number =
  case number of
    Single i ->
      IntSingle.isZero i
    SimpleMulti i ->
      IntSimpleMulti.isZero i
    Hex i ->
      IntHex.isZero i

data Sign
  = Plus
  | Minus

parseSign :: Parser Sign
parseSign =
  [ Data.Attoparsec.ByteString.Char8.char '-' >> pure Minus,
    pure Plus
  ]
    & Data.Attoparsec.ByteString.Char8.choice

parse :: Parser Int_
parse =
  do
    sign <- parseSign
    num <- parsePositive
    pure (Int_ sign num)

parsePositive :: Parser Positive
parsePositive =
  [ fmap Hex IntHex.parse,
    fmap SimpleMulti IntSimpleMulti.parse,
    fmap Single IntSingle.parse
  ]
    & Data.Attoparsec.ByteString.Char8.choice

write :: Int_ -> ByteString
write (Int_ sign positive) =
  writeSign sign positive <> writePositive positive

writeSign :: Sign -> Positive -> ByteString
writeSign sign number =
  case sign of
    Plus ->
      ""
    Minus ->
      if isZero number then
        ""
      else
        "-"

writePositive :: Positive -> ByteString
writePositive int =
  case int of
    Single i ->
      IntSingle.write i
    SimpleMulti i ->
      IntSimpleMulti.write i
    Hex i ->
      IntHex.write i
