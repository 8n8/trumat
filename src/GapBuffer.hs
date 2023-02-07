module GapBuffer
  ( GapBuffer,
    GapBuffer.read,
    insertNewline,
    insertSpace,
    position,
    moveRight,
    delete,
    GapBuffer.new,
    fill,
    get,
  )
where

import Char
import Control.Monad.ST (ST)
import Data.ByteString (ByteString, empty, indexMaybe, length, singleton)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed.Mutable (STVector, new, unsafeRead, unsafeWrite)
import Data.Word (Word8)
import Prelude
  ( Int,
    Maybe (..),
    fmap,
    return,
    ($),
    (*),
    (+),
    (-),
    (/=),
    (<),
    (<>),
    (==),
  )

data GapBuffer s
  = GapBuffer (STVector s Word8) (STRef s Int) (STRef s Int)

position :: GapBuffer s -> ST s Int
position (GapBuffer _ leftRef _) =
  readSTRef leftRef

insertNewline :: GapBuffer s -> ST s ()
insertNewline buffer =
  insertWord buffer 10

insertSpace :: GapBuffer s -> ST s ()
insertSpace buffer =
  insertWord buffer 32

insertWord :: GapBuffer s -> Word8 -> ST s ()
insertWord (GapBuffer buffer leftRef _) word =
  do
    left <- readSTRef leftRef
    unsafeWrite buffer left word
    writeSTRef leftRef (left + 1)

get :: GapBuffer s -> ST s Char
get (GapBuffer buffer _ rightRef) =
  do
    right <- readSTRef rightRef
    if right < 1
      then return AfterEnd
      else do
        word <- unsafeRead buffer (capacity - right - 1)
        return $ Char.fromWord word

read :: GapBuffer s -> ST s ByteString
read (GapBuffer buffer leftRef rightRef) =
  do
    left <- readSTRef leftRef
    right <- readSTRef rightRef
    leftBuf <- readLeftHelp left 0 buffer empty
    rightBuf <- readRightHelp right 0 buffer empty
    return $ leftBuf <> rightBuf

readRightHelp ::
  Int ->
  Int ->
  STVector s Word8 ->
  ByteString ->
  ST s ByteString
readRightHelp size i buffer accum =
  if i < size
    then do
      word <- unsafeRead buffer (capacity - 1 - i)
      readRightHelp size (i + 1) buffer (accum <> singleton word)
    else return accum

readLeftHelp :: Int -> Int -> STVector s Word8 -> ByteString -> ST s ByteString
readLeftHelp size i buffer accum =
  if i < size
    then do
      word <- unsafeRead buffer i
      readLeftHelp size (i + 1) buffer (accum <> singleton word)
    else return accum

capacity :: Int
capacity =
  10 * 1000 * 1000

fill :: ByteString -> GapBuffer s -> ST s (Maybe ())
fill bytes buffer =
  if Data.ByteString.length bytes == 0
    then return Nothing
    else fmap Just $ fillHelp bytes 0 buffer

fillHelp :: ByteString -> Int -> GapBuffer s -> ST s ()
fillHelp bytes i gapBuffer@(GapBuffer buffer left right) =
  case indexMaybe bytes i of
    Nothing ->
      do
        writeSTRef left 0
        writeSTRef right (Data.ByteString.length bytes)
        return ()
    Just byte ->
      let index :: Int
          index = capacity - Data.ByteString.length bytes - 1 + i
       in do
            unsafeWrite buffer index byte
            fillHelp bytes (i + 1) gapBuffer

new :: ST s (GapBuffer s)
new =
  do
    buffer <- Data.Vector.Unboxed.Mutable.new capacity
    left <- newSTRef 0
    right <- newSTRef 0
    return $ GapBuffer buffer left right

delete :: GapBuffer s -> ST s ()
delete (GapBuffer _ _ rightRef) =
  do
    right <- readSTRef rightRef
    if right /= 0
      then writeSTRef rightRef (right - 1)
      else return ()

moveRight :: GapBuffer s -> ST s ()
moveRight (GapBuffer buffer leftRef rightRef) =
  do
    left <- readSTRef leftRef
    right <- readSTRef rightRef
    if right == -1
      then return ()
      else do
        current <- unsafeRead buffer (capacity - 1 - right)
        unsafeWrite buffer left current
        writeSTRef leftRef (left + 1)
        writeSTRef rightRef (right - 1)
