module Characters (Characters, empty, fromFile, get) where

import Array (Array)
import qualified Array
import Control.Exception (try)
import Data.Word (Word8)
import ElmChar
import Result
import System.IO (Handle, hGetChar, hSetBinaryMode)
import System.IO.Error (isEOFError)

data Characters
  = Characters (Array Word8)

get :: Int -> Characters -> IO (Either String ElmChar)
get index (Characters array) =
  do
    getResult <- Array.get index array
    case getResult of
      Left error' ->
        pure (Left error')
      Right raw ->
        pure (ElmChar.parse raw)

capacity :: Int
capacity =
  1000 * 1000

empty :: IO Characters
empty =
  do
    array <- Array.empty capacity
    pure (Characters array)

fromFile :: Handle -> Characters -> IO Result
fromFile handle (Characters characters) =
  do
    hSetBinaryMode handle True
    result <- parseHelp handle characters
    hSetBinaryMode handle False
    return result

parseHelp :: Handle -> Array Word8 -> IO Result
parseHelp handle array =
  do
    eitherWord <- (try $ hGetChar handle) :: IO (Either IOError Char)
    case eitherWord of
      Left error' ->
        if isEOFError error'
          then pure Ok
          else pure (Error ("error reading input file: " <> show error'))
      Right char ->
        case ElmChar.parse (fromIntegral (fromEnum char)) of
          Left error' ->
            pure (Error error')
          Right elmChar ->
            do
              appendResult <- Array.append (ElmChar.encode elmChar) array
              case appendResult of
                Ok ->
                  parseHelp handle array
                Error error' ->
                  pure (Error error')
