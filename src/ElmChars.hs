module ElmChars (ElmChars, Result (..), malloc, parse, zero) where

import Bytes (Bytes)
import qualified Bytes
import qualified ElmChar
import Prelude
  ( IO,
    Int,
    Maybe (..),
    String,
    fmap,
    pure,
    show,
    (+),
    (<>),
  )

newtype ElmChars
  = ElmChars Bytes

zero :: ElmChars -> IO ()
zero (ElmChars bytes) =
  Bytes.zero bytes

malloc :: IO ElmChars
malloc =
  fmap ElmChars Bytes.malloc

data Result
  = Ok
  | Error String

parse :: Bytes -> ElmChars -> IO Result
parse bytes elmChars =
  parseHelp bytes elmChars 0

parseHelp :: Bytes -> ElmChars -> Int -> IO Result
parseHelp bytes (ElmChars elmChars) index =
  do
    Bytes.zero elmChars
    readResult <- Bytes.get index bytes
    case readResult of
      Nothing ->
        pure Ok
      Just byte ->
        case ElmChar.parse byte of
          Nothing ->
            pure (Error ("invalid byte in input file: " <> show byte))
          Just elmChar ->
            do
              writeResult <- Bytes.append elmChars (ElmChar.toByte elmChar)
              case writeResult of
                Bytes.NotEnoughSpace ->
                  pure (Error "not enough space in Elm character array")
                Bytes.Ok ->
                  parseHelp bytes (ElmChars elmChars) (index + 1)
