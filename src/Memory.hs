module Memory (Memory, malloc, elmChars, zero) where

import ElmChars (ElmChars)
import qualified ElmChars
import Prelude (IO, pure, ($))

data Memory = Memory
  { elmChars :: ElmChars
  }

zero :: Memory -> IO ()
zero (Memory elmChars') =
  do
    ElmChars.zero elmChars'

malloc :: IO Memory
malloc =
  do
    elmChars' <- ElmChars.malloc
    pure $ Memory elmChars'
