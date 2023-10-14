module Array
  ( Array,
    zero,
    append,
    Result (..),
    malloc,
    get
  )
where

import Data.IORef (IORef)
import qualified Data.IORef
import qualified Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr)
import qualified Foreign.Storable
import Foreign.Storable (Storable)
import Prelude
  ( IO,
    Int,
    Maybe (..),
    pure,
    ($),
    (+),
    (<),
    (==),
    (>=),
    (||),
  )

zero :: Array a -> IO ()
zero (Array _ sizeReference _) =
  Data.IORef.writeIORef sizeReference 0

data Array a
  = Array (Ptr a) (IORef Int) Int

get :: (Storable a) => Int -> Array a -> IO (Maybe a)
get index (Array pointer sizeReference _) =
  do
    size <- Data.IORef.readIORef sizeReference
    if index < 0 || index >= size
      then pure Nothing
      else do
        word <- Foreign.Storable.peekElemOff pointer index
        pure $ Just word

malloc :: Int -> IO (Array a)
malloc capacity =
  do
    pointer <- Foreign.Marshal.Alloc.mallocBytes capacity
    sizeReference <- Data.IORef.newIORef 0
    pure (Array pointer sizeReference capacity)

data Result
  = Ok
  | NotEnoughSpace

append :: (Storable a) => Array a -> a -> IO Result
append (Array pointer sizeReference capacity) word =
  do
    size <- Data.IORef.readIORef sizeReference
    if size == capacity
      then pure NotEnoughSpace
      else do
        Foreign.Storable.pokeElemOff pointer size word
        Data.IORef.writeIORef sizeReference (size + 1)
        pure Ok
