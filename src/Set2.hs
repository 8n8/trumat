module Set2 (Set2, empty) where

import qualified Array
import Array (Array)
import Data.Word (Word32)

data Set2
    = Set2 (Array Word32) (Array Word32)


empty :: Int -> IO Set2
empty capacity =
    do
    one <- Array.empty capacity
    two <- Array.empty capacity
    pure $ Set2 one two
