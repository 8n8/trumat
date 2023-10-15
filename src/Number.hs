module Number (Number, parse) where

import Digit (Digit)
import qualified Digit
import Prelude (Maybe(..), Int, IO, (+), pure)
import qualified ElmChars
import ElmChars (ElmChars)

data Number
  = Integer Digit


parse :: Int -> ElmChars -> IO (Maybe (Number, Int))
parse index chars =
  do
  result <- ElmChars.get index chars
  case result of
    Nothing ->
      pure Nothing

    Just char ->
      case Digit.parse char of
        Nothing ->
          pure Nothing
        Just digit ->
          pure (Just (Integer digit, index + 1))
