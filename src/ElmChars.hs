module ElmChars (ElmChars, Result (..), malloc, parse, zero, get) where

import Bytes (Bytes)
import qualified Bytes
import qualified ElmChar
import ElmChar (ElmChar)
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

get :: Int -> ElmChars -> IO (Maybe ElmChar)
get index (ElmChars bytes) =
  do
  result <- Bytes.get index bytes
  case result of
    Nothing ->
      pure Nothing

    Just rawByte ->
      case ElmChar.parse rawByte of
        Nothing ->
          pure Nothing

        Just elmChar ->
          pure (Just elmChar)

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
