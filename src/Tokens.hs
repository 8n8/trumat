module Tokens (Tokens) where

import Bytes (Bytes)
import Number (Number)
import Data.Word (Word32, Word8)
import Array (Array)
import UpperName (UpperName)
import LowerName (LowerName)

data Token
  = Module
  | Exposing
  | LowerName LowerName
  | UpperName UpperName
  | OpenParentheses
  | CloseParentheses
  | Newline
  | Space
  | Equals
  | Number Number

newtype Tokens
  = Tokens
      -- 9 bytes per token
      (Array Word8) -- tag
      (Array Word32) -- source start
      (Array Word32) -- source end
