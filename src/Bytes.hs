module Bytes (Bytes, new, get, append) where

import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeElemOff, peekElemOff)
import Foreign.Marshal.Alloc (mallocBytes)

capacity :: Int
capacity =
    5*1000*1000

data Bytes
    = Bytes (Ptr Word8) (IORef Int)

append :: Word8 -> Bytes -> IO (Maybe ())
append word (Bytes bytes sizeRef) =
    do
    size <- readIORef sizeRef
    if size == capacity then
        return Nothing

    else
        do
        pokeElemOff bytes size word
        writeIORef sizeRef (size + 1)
        return $ Just ()

get :: Int -> Bytes -> IO (Maybe Word8)
get index (Bytes bytes sizeRef) =
    do
    size <- readIORef sizeRef
    if index >= 0 && index < size then
        do
        word <- peekElemOff bytes index
        return $ Just word

    else
        return Nothing

new :: IO (Maybe Bytes)
new =
    do
    bytes <- mallocBytes capacity
    sizeRef <- newIORef 0
    return $ Just $ Bytes bytes sizeRef
