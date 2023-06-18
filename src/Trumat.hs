module Trumat (trumat) where

import Data.Text (Text)
import qualified Data.Text as Text
import ElmChar (ElmChar)
import qualified ElmChar

trumat :: Text -> Either String Text
trumat unformatted =
  case parseCharacters unformatted of
    Nothing ->
      Left "couldn't parse characters"
    Just characters ->
      Right $ charactersToString characters

charactersToString :: [ElmChar] -> Text
charactersToString chars =
  Text.pack $ map ElmChar.toChar chars

parseCharacters :: Text -> Maybe [ElmChar]
parseCharacters text =
  onlyJusts $ map ElmChar.fromChar $ Text.unpack text

onlyJusts :: [Maybe a] -> Maybe [a]
onlyJusts maybes =
  onlyJustsHelp maybes []

onlyJustsHelp :: [Maybe a] -> [a] -> Maybe [a]
onlyJustsHelp maybes accumulator =
  case maybes of
    Nothing : _ ->
      Nothing
    Just a : tail_ ->
      onlyJustsHelp tail_ (a : accumulator)
    [] ->
      Just $ reverse accumulator
