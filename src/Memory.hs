module Memory (Memory, malloc) where

import Prelude (IO, pure, ($))
import Tokens (Tokens)
import qualified Tokens

data Memory
  = Memory Tokens

malloc :: IO Memory
malloc =
  do
  tokens <- Tokens.malloc
  pure $ Memory tokens
