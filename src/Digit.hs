module Digit (Digit(..), fromChar, toChar) where


data Digit
    = D0


toChar :: Digit -> Char
toChar digit =
    case digit of
        D0 ->
            '0'


fromChar :: Char -> Maybe Digit
fromChar char =
    case char of
        '0' ->
            Just D0

        _ ->
            Nothing
