module Expression (Expression, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import Float_ (Float_)
import qualified Float_
import Int_ (Int_)
import qualified Int_

data Expression
  = Int_ Int_
  | Float_ Float_

parse :: Parser Expression
parse =
  [ fmap Float_ Float_.parse,
    fmap Int_ Int_.parse
  ]
    & Data.Attoparsec.ByteString.Char8.choice

write :: Expression -> ByteString
write expression =
  case expression of
    Int_ i ->
      Int_.write i
    Float_ f ->
      Float_.write f
