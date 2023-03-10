module Array8 (Array8, malloc, push, get) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

data Array8
  = Array8 (Ptr Word8) (IORef Int) Int

get :: Int -> Array8 -> IO (Maybe Word8)
get index (Array8 buffer sizeRef _) =
  do
    size <- readIORef sizeRef
    if index >= size
      then return Nothing
      else do
        word <- peekElemOff buffer index
        return $ Just word

push :: Word8 -> Array8 -> IO (Maybe ())
push word (Array8 buffer sizeRef capacity) =
  do
    size <- readIORef sizeRef
    if size == capacity
      then return Nothing
      else do
        pokeElemOff buffer size word
        writeIORef sizeRef (size + 1)
        return $ Just ()

malloc :: Int -> IO Array8
malloc capacity =
  do
    buffer <- mallocBytes capacity
    sizeRef <- newIORef 0
    return $ Array8 buffer sizeRef capacity
