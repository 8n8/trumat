module Memory (Memory, empty, characters, tokens, rows) where

import Characters (Characters)
import qualified Characters
import Rows (Rows)
import qualified Rows
import Tokens (Tokens)
import qualified Tokens

data Memory = Memory
  { characters :: Characters,
    tokens :: Tokens,
    rows :: Rows
  }

empty :: IO Memory
empty =
  do
    characters <- Characters.empty
    tokens <- Tokens.empty
    rows <- Rows.empty
    pure $ Memory characters tokens rows
