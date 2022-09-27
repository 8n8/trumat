module Src (Src, readModule) where

import qualified Data.Vector.Unboxed.Mutable
import qualified Data.Word
import qualified System.IO
import qualified System.IO.Error
import qualified Data.IORef
import qualified Control.Exception
import qualified SrcLength


data Src
    = Src
        (Data.Vector.Unboxed.Mutable.IOVector Data.Word.Word8)
        (Data.IORef.IORef SrcLength.SrcLength)


readModule :: System.IO.Handle -> Src -> IO ()
readModule handle src =
    do
    System.IO.hSetBinaryMode handle True
    readModuleHelp handle src


readModuleHelp :: System.IO.Handle -> Src -> IO ()
readModuleHelp handle src =
    do
    maybeChar <- getByte handle
    case maybeChar of
        Nothing ->
            addChar moduleEnd src

        Just char ->
            do
            addChar char src
            readModuleHelp handle src


addChar :: Data.Word.Word8 -> Src -> IO ()
addChar char (Src buffer lengthIo) =
    do
    oldLength <- Data.IORef.readIORef lengthIo
    Data.Vector.Unboxed.Mutable.write
        buffer
        (SrcLength.toInt oldLength)
        char
    case SrcLength.plus1 oldLength of
        Nothing ->
            Control.Exception.throwIO (System.IO.Error.userError "code base is too large")
        Just newLength ->
            Data.IORef.writeIORef lengthIo newLength

            
moduleEnd :: Data.Word.Word8
moduleEnd =
    255


getByte :: System.IO.Handle -> IO (Maybe Data.Word.Word8)
getByte handle =
    do
    result <- Control.Exception.try (System.IO.hGetChar handle)
    case result of
        Left err ->
            if System.IO.Error.isEOFError err then
                pure Nothing

            else
                Control.Exception.throwIO err

        Right char ->
            pure (Just (fromIntegral (fromEnum char)))
