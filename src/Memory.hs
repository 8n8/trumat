module Memory (Memory, malloc, elmChars) where

import ElmChars (ElmChars)
import qualified ElmChars
import Prelude (IO, pure, ($))

data Memory = Memory
  { elmChars :: ElmChars
  }

malloc :: IO Memory
malloc =
  do
    elmChars' <- ElmChars.malloc
    pure $ Memory elmChars'
