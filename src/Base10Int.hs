module Base10Int (Base10Int, parse, toString) where

import Digit (Digit)

data Base10Int
  = Base10Int Digit [Digit]
