module ElmChar (ElmChar(..), toChar, fromChar) where

import Alphabet (Alphabet)
import qualified Alphabet
import Digit (Digit)
import qualified Digit
import Data.Text (Text)
import qualified Data.Text as Text

data ElmChar
    = Uppercase Alphabet
    | Lowercase Alphabet
    | Digit Digit
    | Space
    | Newline
    | OpenParenthesis
    | CloseParenthesis
    | Equals


fromChar :: Char -> Maybe ElmChar
fromChar char =
    case Alphabet.fromLowercase char of
        Just lowercase ->
            Just (Lowercase lowercase)
        Nothing ->
            case Alphabet.fromUppercase char of
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
            Alphabet.toUppercase alphabet

        Lowercase alphabet ->
            Alphabet.toLowercase alphabet
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

