module ElmChar (ElmChar (..), toChar, fromChar) where

import Data.Text (Text)
import qualified Data.Text as Text
import Digit (Digit)
import qualified Digit
import Lowercase (Lowercase)
import qualified Lowercase
import Uppercase (Uppercase)
import qualified Uppercase

data ElmChar
  = Uppercase Uppercase
  | Lowercase Lowercase
  | Digit Digit
  | Space
  | Newline
  | OpenParenthesis
  | CloseParenthesis
  | Equals

fromChar :: Char -> Maybe ElmChar
fromChar char =
  case Lowercase.fromChar char of
    Just lowercase ->
      Just (Lowercase lowercase)
    Nothing ->
      case Uppercase.fromChar char of
        Just uppercase ->
          Just (Uppercase uppercase)
        Nothing ->
          case Digit.fromChar char of
            Just digit ->
              Just (Digit digit)
            Nothing ->
              case char of
                ' ' ->
                  Just Space
                '\n' ->
                  Just Newline
                '(' ->
                  Just OpenParenthesis
                ')' ->
                  Just CloseParenthesis
                '=' ->
                  Just Equals
                _ ->
                  Nothing

toChar :: ElmChar -> Char
toChar char =
  case char of
    Digit digit ->
      Digit.toChar digit
    Uppercase alphabet ->
      Uppercase.toChar alphabet
    Lowercase alphabet ->
      Lowercase.toChar alphabet
    Space ->
      ' '
    Newline ->
      '\n'
    OpenParenthesis ->
      '('
    CloseParenthesis ->
      ')'
    Equals ->
      '='
