module List (List, new) where

import Data.IORef (IORef, newIORef)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (mallocBytes)


data List
    = List (Ptr Word32) (Ptr Word32) (IORef Int)


capacity :: Int
capacity =
    50*1000


new :: IO List
new =
    do
    listId <- mallocBytes (4 * capacity)
    itemId <- mallocBytes (4 * capacity)
    size <- newIORef 0
    return $ List listId itemId size
