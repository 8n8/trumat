module Memory (Memory, empty, characters, tokens, rows, columns) where

import Characters (Characters)
import qualified Characters
import Columns (Columns)
import qualified Columns
import Rows (Rows)
import qualified Rows
import Tokens (Tokens)
import qualified Tokens

data Memory = Memory
  { characters :: Characters,
    tokens :: Tokens,
    rows :: Rows,
    columns :: Columns
  }

empty :: IO Memory
empty =
  do
    characters <- Characters.empty
    tokens <- Tokens.empty
    rows <- Rows.empty
    columns <- Columns.empty
    pure $ Memory characters tokens rows columns
