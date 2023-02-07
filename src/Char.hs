module Char (Char (..), fromWord) where

import Data.Word (Word8)
import Prelude (Show)

data Char
  = CloseBracket
  | OpenBracket
  | Space
  | Newline
  | Comma
  | Equals
  | AfterEnd
  | Other
  deriving (Show)

fromWord :: Word8 -> Char
fromWord word =
  case word of
    10 ->
      Newline
    32 ->
      Space
    44 ->
      Comma
    61 ->
      Equals
    91 ->
      OpenBracket
    93 ->
      CloseBracket
    _ ->
      Other
