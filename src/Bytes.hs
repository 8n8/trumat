module Bytes
  ( Bytes,
    zero,
    append,
    Result (..),
    malloc,
    get,
    readFile,
    writeFile,
  )
where

import Data.IORef (IORef)
import qualified Data.IORef
import Data.Word (Word8)
import qualified Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr)
import qualified Foreign.Storable
import qualified System.IO
import Prelude
  ( FilePath,
    IO,
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

capacity :: Int
capacity =
  1000000

zero :: Bytes -> IO ()
zero (Bytes _ sizeReference) =
  Data.IORef.writeIORef sizeReference 0

data Bytes
  = Bytes (Ptr Word8) (IORef Int)

readFile :: FilePath -> Bytes -> IO ()
readFile path (Bytes pointer sizeReference) =
  System.IO.withFile path System.IO.ReadMode $ \handle ->
    do
      n <- System.IO.hGetBuf handle pointer capacity
      Data.IORef.writeIORef sizeReference n

writeFile :: Bytes -> FilePath -> IO ()
writeFile (Bytes pointer sizeReference) path =
  System.IO.withFile path System.IO.WriteMode $ \handle ->
    do
      size <- Data.IORef.readIORef sizeReference
      System.IO.hPutBuf handle pointer size

get :: Int -> Bytes -> IO (Maybe Word8)
get index (Bytes pointer sizeReference) =
  do
    size <- Data.IORef.readIORef sizeReference
    if index < 0 || index >= size
      then pure Nothing
      else do
        word <- Foreign.Storable.peekElemOff pointer index
        pure $ Just word

malloc :: IO Bytes
malloc =
  do
    pointer <- Foreign.Marshal.Alloc.mallocBytes capacity
    sizeReference <- Data.IORef.newIORef 0
    pure (Bytes pointer sizeReference)

data Result
  = Ok
  | NotEnoughSpace

append :: Bytes -> Word8 -> IO Result
append (Bytes pointer sizeReference) word =
  do
    size <- Data.IORef.readIORef sizeReference
    if size == capacity
      then pure NotEnoughSpace
      else do
        Foreign.Storable.pokeElemOff pointer size word
        Data.IORef.writeIORef sizeReference (size + 1)
        pure Ok
