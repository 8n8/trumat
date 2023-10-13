module Memory (Memory, malloc) where

import Prelude (IO, pure)

data Memory
  = Memory

malloc :: IO Memory
malloc =
  pure Memory
