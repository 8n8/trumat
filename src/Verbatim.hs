module Verbatim (Verbatim, new) where

import qualified Capacity
import qualified Array
import Array (Array)
import Data.Word (Word32)

data Verbatim
    = Verbatim (Array Word32) (Array Word32)


new :: IO Verbatim
new =
    do
    start <- Array.new Capacity.k500
    end <- Array.new Capacity.k500
    return $ Verbatim start end
