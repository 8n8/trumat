module ElmChar (ElmChar(..), parse) where

import Prelude
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, token)


type Parser =
  Parsec Void Text

data ElmChar
    = Au | Bu | Cu | Du | Eu | Fu | Gu | Hu | Iu | Ju | Ku | Lu | Mu | Nu | Ou | Pu | Qu | Ru | Su | Tu | Uu | Vu | Wu | Xu | Yu | Zu | Al | Bl | Cl | Dl | El | Fl | Gl | Hl | Il | Jl | Kl | Ll | Ml | Nl | Ol | Pl | Ql | Rl | Sl | Tl | Ul | Vl | Wl | Xl | Yl | Zl | Space | Newline | DoubleQuote | SingleQuote | Pipe | OpenCurly | CloseCurly | At | Equals | Hyphen | CloseParens | Comma | OpenParens | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
    deriving (Show)

parseChar :: Char -> Maybe ElmChar
parseChar ch =
    case ch of
    ' ' -> Just Space
    '\n' -> Just Newline
    'a' -> Just Al
    'b' -> Just Bl
    'c' -> Just Cl
    'd' -> Just Dl
    'e' -> Just El
    'f' -> Just Fl
    'g' -> Just Gl
    'h' -> Just Hl
    'i' -> Just Il
    'j' -> Just Jl
    'k' -> Just Kl
    'l' -> Just Ll
    'm' -> Just Ml
    'n' -> Just Nl
    'o' -> Just Ol
    'p' -> Just Pl
    'q' -> Just Ql
    'r' -> Just Rl
    's' -> Just Sl
    't' -> Just Tl
    'u' -> Just Ul
    'v' -> Just Vl
    'w' -> Just Wl
    'x' -> Just Xl
    'y' -> Just Yl
    'z' -> Just Zl
    'A' -> Just Au
    'B' -> Just Bu
    'C' -> Just Cu
    'D' -> Just Du
    'E' -> Just Eu
    'F' -> Just Fu
    'G' -> Just Gu
    'H' -> Just Hu
    'I' -> Just Iu
    'J' -> Just Ju
    'K' -> Just Ku
    'L' -> Just Lu
    'M' -> Just Mu
    'N' -> Just Nu
    'O' -> Just Ou
    'P' -> Just Pu
    'Q' -> Just Qu
    'R' -> Just Ru
    'S' -> Just Su
    'T' -> Just Tu
    'U' -> Just Uu
    'V' -> Just Vu
    'W' -> Just Wu
    'X' -> Just Xu
    'Y' -> Just Yu
    'Z' -> Just Zu
    '@' -> Just At
    '(' -> Just OpenParens
    ',' -> Just Comma
    ')' -> Just CloseParens
    '{' -> Just OpenCurly
    '}' -> Just CloseCurly
    '-' -> Just Hyphen
    '|' -> Just Pipe
    '=' -> Just Equals
    '0' -> Just D0
    '1' -> Just D1
    '2' -> Just D2
    '3' -> Just D3
    '4' -> Just D4
    '5' -> Just D5
    '6' -> Just D6
    '7' -> Just D7
    '8' -> Just D8
    '9' -> Just D9
    _ -> Nothing


parse :: Parser ElmChar
parse =
    do
    ch <- token Just Set.empty
    case parseChar ch of
        Nothing ->
            fail ("invalid character: " ++ [ch])

        Just parsed ->
            return parsed

