module Effect (readPureWith, Cmd(..)) where

import Array (Array)
import Data.Word (Word8)
import qualified Array
import qualified Data.ByteString
import Data.ByteString (ByteString)
import qualified Control.Exception
-- import Foreign.Storable (Storable)
import qualified System.IO.Unsafe

data Cmd msg
  = Append (Array Word8) Word8
  | Write (Array Word8) Int Word8
  | Batch [Cmd msg]
  | ReadFile FilePath (Either IOError ByteString -> msg)

-- readArrayPure :: Storable a => Int -> Array a -> Maybe a
readArrayPure i array =
  System.IO.Unsafe.unsafePerformIO $ Array.read array i

-- readPureWith
--   :: Storable a
--   => model
--   -> Cmd msg a
--   -> (model -> msg -> (Int -> Array a -> Maybe a) -> (model, Cmd msg a))
--   -> IO ()
readPureWith model cmd update =
  case cmd of
    Batch cmds ->
      mapM_ (\cmd_ -> readPureWith model cmd_ update) cmds

    Append array a ->
      do
      _ <- Array.append array a
      pure ()

    Write array i a ->
      do
      _ <- Array.write array i a
      pure ()

    ReadFile path msg ->
      do  bytes <- Control.Exception.try (Data.ByteString.readFile path)
          let (newModel, newCmd) = update model (msg bytes) readArrayPure
          readPureWith newModel newCmd update
