module Expression (Expression, new) where

import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Data.IORef (IORef, newIORef)
import Foreign.Marshal.Alloc (mallocBytes)

capacity :: Int
capacity =
    50*1000

data Expression
    = Expression (Ptr Word8) (Ptr Word32) (IORef Int)


new :: IO Expression
new =
    do
    type_ <- mallocBytes capacity
    id_ <- mallocBytes (4 * capacity)
    size <- newIORef 0
    return $ Expression type_ id_ size
