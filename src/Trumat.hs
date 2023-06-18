module Trumat (trumat) where

import Data.Text (Text)
import qualified Data.Text as Text
import ElmChar (ElmChar)
import qualified ElmChar
import qualified Token
import Token (Token)

trumat :: Text -> Either String Text
trumat unformatted =
  case parseCharacters unformatted of
    Nothing ->
      Left "couldn't parse characters"
    Just characters ->
      case tokenize characters [] of
        Nothing ->
          Left "couldn't tokenize"
        Right tokens ->
          tokensToString tokens

tokenize :: [ElmChar] -> [Token] -> Maybe [Token]
tokenize chars tokens =
    case NonEmpty.fromString chars of
        Nothing ->
            Just (reverse tokens)

        nonEmpty ->
            case Token.parse nonEmpty of
                Nothing ->
                    Nothing

                Just (token, remainder) ->
                    tokenize remainder (token : tokens)

tokensToString :: [Token] -> Text
tokensToString tokens =
  mconcat (map Token.toString tokens)

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
