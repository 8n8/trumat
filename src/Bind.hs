module Bind (Bind, new) where

import Data.Word (Word32)
import qualified Capacity
import qualified Array
import Array (Array)

data Bind
    = Bind (Array Word32) (Array Word32)

new :: IO Bind
new =
    do
    left <- Array.new Capacity.k20
    right <- Array.new Capacity.k20
    return $ Bind left right
