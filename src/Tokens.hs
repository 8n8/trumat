module Tokens (Tokens, malloc) where

import Data.Word (Word8)
import Array (Array)
import qualified Array
import Prelude (IO, ($), pure)

newtype Tokens
  = Tokens (Array Word8)

malloc :: IO Tokens
malloc =
  do
  array <- Array.malloc 100000
  pure $ Tokens array
