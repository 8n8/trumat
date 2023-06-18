module IntLiteral (IntLiteral, parse, toString) where

import Data.Text (Text)
import qualified Data.Text as Text
import Digit (Digit)
import qualified Digit
import ElmChar (ElmChar)
import qualified ElmChar

data IntLiteral
  = Base10Literal Digit [Digit]

toString :: IntLiteral -> Text
toString (Base10Literal first remainder) =
  Text.pack $
    Digit.toChar first
      : map Digit.toChar (reverse remainder)

data Parser
  = Init
  | Failed
  | Finished IntLiteral
  | IsBase10 Digit [Digit]

parse :: ElmChar -> Parser -> Parser
parse char parser =
  case parser of
    Init ->
      case parseDigit char of
        Nothing ->
          Failed
        Just base10Char ->
          IsBase10 base10Char []
    Failed ->
      Failed
    Finished finished ->
      Finished finished
    IsBase10 first remainder ->
      case parseDigit char of
        Nothing ->
          if isAfterIntLiteralChar char
            then Finished (Base10Literal first remainder)
            else Failed
        Just base10Char ->
          IsBase10 first (base10Char : remainder)

parseDigit :: ElmChar -> Maybe Digit
parseDigit char =
  case char of
    ElmChar.Digit digit ->
      Just digit
    ElmChar.Uppercase _ ->
      Nothing
    ElmChar.Lowercase _ ->
      Nothing
    ElmChar.Space ->
      Nothing
    ElmChar.Newline ->
      Nothing
    ElmChar.OpenParenthesis ->
      Nothing
    ElmChar.CloseParenthesis ->
      Nothing
    ElmChar.Equals ->
      Nothing

isAfterIntLiteralChar :: ElmChar -> Bool
isAfterIntLiteralChar char =
  case char of
    ElmChar.Digit _ ->
      False
    ElmChar.Uppercase _ ->
      False
    ElmChar.Lowercase _ ->
      False
    ElmChar.Space ->
      True
    ElmChar.Newline ->
      True
    ElmChar.OpenParenthesis ->
      False
    ElmChar.CloseParenthesis ->
      True
    ElmChar.Equals ->
      True
