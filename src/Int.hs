module Int (Int, parse, toString) where


import qualified Prelude
import Base10Int (Base10Int)


data Int
  = Base10 Base10Int
