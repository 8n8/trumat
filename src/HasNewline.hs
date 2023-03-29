module HasNewline (HasNewline, new) where

import Data.IORef (newIORef, IORef)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (mallocBytes)

data HasNewline
    = HasNewline (Ptr Word32) (IORef Int)

capacity :: Int
capacity =
    20*1000

new :: IO HasNewline
new =
    do
    id_ <- mallocBytes (4 * capacity)
    size <- newIORef 0
    return $ HasNewline id_ size
