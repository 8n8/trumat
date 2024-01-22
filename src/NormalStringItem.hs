module NormalStringItem (NormalStringItem, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data NormalStringItem
  = Aa
  | Bb
  | Cc
  | Space

parse :: Parser NormalStringItem
parse =
  do
    ch <- Data.Attoparsec.ByteString.Char8.anyChar
    case charToItem ch of
      Nothing ->
        fail "Invalid normal string item"
      Just item ->
        pure item

charToItem :: Char -> Maybe NormalStringItem
charToItem ch =
  case ch of
    'a' ->
      Just Aa
    'b' ->
      Just Bb
    'c' ->
      Just Cc
    ' ' ->
      Just Space
    _ ->
      Nothing

write :: NormalStringItem -> ByteString
write item =
  case item of
    Aa ->
      "a"
    Bb ->
      "b"
    Cc ->
      "c"
    Space ->
      " "
