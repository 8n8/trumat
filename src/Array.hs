module Array (Array, new, get, append) where

import Capacity (Capacity)
import qualified Capacity
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Foreign.Storable (pokeElemOff, Storable, peekElemOff)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (mallocArray)

data Array a
    = Array (Ptr a) (IORef Int) Capacity

append :: Storable a => a -> Array a -> IO (Maybe ())
append item (Array array sizeRef capacity) =
    do
    size <- readIORef sizeRef
    if size == Capacity.toInt capacity then
        return Nothing

    else
        do
        pokeElemOff array size item
        writeIORef sizeRef (size + 1)
        return (Just ())

new :: Storable a => Capacity -> IO (Array a)
new capacity =
    do
    array <- mallocArray (Capacity.toInt capacity)
    size <- newIORef 0
    return $ Array array size capacity

get :: Storable a => Int -> Array a -> IO (Maybe a)
get index (Array array sizeRef _) =
    do
    size <- readIORef sizeRef
    if index < 0 || index >= size then
        return Nothing

    else
        do
        item <- peekElemOff array index
        return $ Just item
