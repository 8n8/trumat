module Trumat (format, Result (..)) where

import Bytes (Bytes)
import qualified Bytes
import qualified Data.Char
import Data.Word (Word8)
import qualified ElmChars
import Memory (Memory)
import qualified Memory
import qualified Tokens
import Prelude
  ( Char,
    Eq,
    IO,
    Maybe (..),
    Show,
    String,
    fromIntegral,
    pure,
    (<),
    (<>),
  )

data Result
  = Ok
  | Error String
  deriving (Eq, Show)

format :: Memory -> Bytes -> Bytes -> IO Result
format memory input out =
  do
    charParseResult <- ElmChars.parse input (Memory.elmChars memory)
    case charParseResult of
      ElmChars.Error message ->
        pure (Error message)
      ElmChars.Ok ->
        do
        tokeniseResult <- Tokens.parse (Memory.elmChars memory) (Memory.tokens memory)
        case tokeniseResult of
          Tokens.Error message ->
            pure (Error message)

          Tokens.Ok ->
            appendString
              out
              "module X exposing (x)\n\
              \\n\
              \\n\
              \x =\n\
              \    0\n\
              \"

appendString :: Bytes -> String -> IO Result
appendString bytes string =
  case string of
    top : remainder ->
      case charToWord8 top of
        Nothing ->
          pure (Error ("invalid character: " <> [top]))
        Just word ->
          do
            result <- Bytes.append bytes word
            case result of
              Bytes.Ok ->
                appendString bytes remainder
              Bytes.NotEnoughSpace ->
                pure (Error "not enough space in buffer")
    [] ->
      pure Ok

charToWord8 :: Char -> Maybe Word8
charToWord8 char =
  if Data.Char.ord char < 256
    then Just (fromIntegral (Data.Char.ord char))
    else Nothing
