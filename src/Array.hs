module Array (Array, append, empty, read, write) where

import Foreign.Ptr (Ptr)
import Prelude (Maybe(Nothing, Just), IO, Int, pure, (>=), (||), (<), (+), (==))
import qualified Foreign.Storable
import Foreign.Storable (Storable)
import qualified Foreign.Marshal.Array
import Capacity (Capacity)
import qualified Capacity

data Array a
  = Array Int (Ptr a) Capacity
  | Empty Capacity

append :: (Storable a) => Array a -> a -> IO (Maybe (Array a))
append array value =
  case array of
    Empty capacity ->
        do
        ptr <- Foreign.Marshal.Array.mallocArray (Capacity.toInt capacity)
        Foreign.Storable.pokeElemOff ptr 0 value
        pure (Just (Array 0 ptr capacity))

    Array length ptr capacity ->
      if length == Capacity.toInt capacity then
        pure Nothing
      else
      do
      Foreign.Storable.pokeElemOff ptr length value
      pure (Just (Array (length + 1) ptr capacity))

empty :: Capacity -> Array a
empty capacity =
  Empty capacity

read :: (Storable a) => Array a -> Int -> IO (Maybe a)
read array index =
  case array of
    Empty _ ->
      pure Nothing

    Array length ptr _ ->
      if index < 0 || index >= length then
        pure Nothing
      else
        do
        value <- Foreign.Storable.peekElemOff ptr index
        pure (Just value)

write :: (Storable a) => Array a -> Int -> a -> IO (Maybe ())
write array index value =
  case array of
    Empty _ ->
      pure Nothing

    Array length ptr _ ->
      if index < 0 || index >= length then
        pure Nothing
      else
        do
        Foreign.Storable.pokeElemOff ptr index value
        pure (Just ())
