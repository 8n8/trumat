module Trumat (trumat) where

import qualified ElmChar
import ElmChar (ElmChar)
import Data.Text (Text)
import qualified Data.Text as Text

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
