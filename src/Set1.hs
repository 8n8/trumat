module Set1 (Set1, empty) where

import Array (Array)
import qualified Array
import Data.Word (Word32)

data Set1
    = Set1 (Array Word32)


empty :: Int -> IO Set1
empty capacity =
    fmap Set1 (Array.empty capacity)
