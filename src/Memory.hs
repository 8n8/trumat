module Memory (Memory, empty, characters, tokens) where

import Characters (Characters)
import qualified Characters
import Tokens (Tokens)
import qualified Tokens

data Memory = Memory
  { characters :: Characters,
    tokens :: Tokens
  }

empty :: IO Memory
empty =
  do
    characters <- Characters.empty
    tokens <- Tokens.empty
    pure $ Memory characters tokens
