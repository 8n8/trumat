module Digit (Digit, parse, toString) where


import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec
import qualified Data.Set

type Parser = Parsec Void Text

data Digit
  = D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9


parse :: Parser Digit
parse =
  do
  char <- Text.Megaparsec.token Just Data.Set.empty
  case char of
    '0' -> pure D0
    '1' -> pure D1
    '2' -> pure D2
    '3' -> pure D3
    '4' -> pure D4
    '5' -> pure D5
    '6' -> pure D6
    '7' -> pure D7
    '8' -> pure D8
    '9' -> pure D9
    _ -> fail "Expected a digit" 


toString :: Digit -> Text
toString digit =
  case digit of
    D1 -> "1"
    D2 -> "2"
    D3 -> "3"
    D4 -> "4"
    D5 -> "5"
    D6 -> "6"
    D7 -> "7"
    D8 -> "8"
    D9 -> "9"
    D0 -> "0"
