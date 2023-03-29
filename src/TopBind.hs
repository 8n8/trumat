module TopBind (TopBind, new) where

import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.IORef (IORef, newIORef)
import Foreign.Marshal.Alloc (mallocBytes)


data TopBind
    = TopBind (Ptr Word32) (IORef Int)


capacity :: Int
capacity =
    10*1000


new :: IO TopBind
new =
    do
    id_ <- mallocBytes (4 * capacity)
    size <- newIORef 0
    return $ TopBind id_ size
