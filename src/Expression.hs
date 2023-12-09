module Expression (Expression, parse, toString) where

import qualified Prelude
import Int (Int)
import qualified Int

data Expression
  = Int Int
