module Array8 (Array8, malloc, get, Index, set, makeIndex) where

import Data.Word (Word8)
import Data.IORef (IORef, readIORef, newIORef)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import Foreign.Marshal.Alloc (mallocBytes)

data Array8
    = Array8 (Ptr Word8) (IORef Int) Int

data Index
    = Index (Ptr Word8) Int

makeIndex :: Array8 -> Int -> IO (Maybe Index)
makeIndex (Array8 buffer sizeRef _) candidate =
    do
    size <- readIORef sizeRef
    if candidate >= size || candidate < 0 then
        return Nothing

    else
        return $ Just $ Index buffer candidate

malloc :: Int -> IO Array8
malloc capacity =
  do
    buffer <- mallocBytes capacity
    size <- newIORef 0
    return $ Array8 buffer size capacity

get :: Index -> IO Word8
get (Index buffer index) =
    peekElemOff buffer index

set :: Word8 -> Index -> IO ()
set word (Index buffer index) =
    pokeElemOff buffer index word
