module Export (Export, new) where


import qualified Capacity
import qualified Array
import Array (Array)
import Data.Word (Word32)

data Export
    = Export (Array Word32) (Array Word32)


new :: IO Export
new =
    do
    export <- Array.new Capacity.k2
    exposed <- Array.new Capacity.k2
    return $ Export export exposed
