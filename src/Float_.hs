module Float_ (Float_, write, parse) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Function ((&))
import FloatExponent (FloatExponent)
import qualified FloatExponent
import FloatSimple (FloatSimple)
import qualified FloatSimple

data Float_
  = Simple FloatSimple
  | Exponent FloatExponent

write :: Float_ -> ByteString
write float =
  case float of
    Simple f ->
      FloatSimple.write f
    Exponent f ->
      FloatExponent.write f

parse :: Parser Float_
parse =
  [ fmap Exponent FloatExponent.parse,
    fmap Simple FloatSimple.parse
  ]
    & Data.Attoparsec.ByteString.Char8.choice
