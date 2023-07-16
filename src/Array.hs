module Array (Array, empty, append, get) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peekElemOff, pokeElemOff)
import Result

data Array item = Array
  { items :: Ptr item,
    lengthRef :: IORef Int,
    capacity :: Int
  }

get :: Storable item => Int -> Array item -> IO (Either String item)
get index array =
  do
    len <- readIORef (lengthRef array)
    if index < 0 || index >= len
      then pure (Left ("invalid array index: " <> show index))
      else do
        item <- peekElemOff (items array) index
        pure (Right item)

empty :: Storable item => Int -> IO (Array item)
empty capacity =
  do
    ptr <- mallocArray capacity
    lengthRef <- newIORef 0
    return $ Array {items = ptr, lengthRef = lengthRef, capacity = capacity}

append :: Storable item => item -> Array item -> IO Result
append item array =
  do
    len <- readIORef (lengthRef array)
    if len == (capacity array)
      then pure (Error "array is full")
      else do
        pokeElemOff (items array) len item
        writeIORef (lengthRef array) (len + 1)
        pure Ok
