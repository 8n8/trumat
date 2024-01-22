module NormalStringItem (NormalStringItem, parse, write) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

data NormalStringItem
  = Aa
  | Bb
  | Cc
  | Dd
  | Ee
  | Ff
  | Gg
  | Hh
  | Ii
  | Jj
  | Kk
  | Ll
  | Mm
  | Nn
  | Oo
  | Pp
  | Qq
  | Rr
  | Ss
  | Tt
  | Uu
  | Vv
  | Ww
  | Xx
  | Yy
  | Zz
  | AA
  | BB
  | CC
  | DD
  | EE
  | FF
  | GG
  | HH
  | II
  | JJ
  | KK
  | LL
  | MM
  | NN
  | OO
  | PP
  | QQ
  | RR
  | SS
  | TT
  | UU
  | VV
  | WW
  | XX
  | YY
  | ZZ
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Zero
  | Space
  | ExclamationMark
  | Hash
  | Dollar
  | Percent
  | Ampersand
  | Apostrophe
  | LeftParenthesis
  | RightParenthesis
  | Asterisk
  | Plus
  | Comma
  | Minus
  | Dot
  | Slash
  | Colon
  | Semicolon
  | LessThan
  | Equal
  | GreaterThan
  | QuestionMark
  | At
  | LeftSquareBracket
  | RightSquareBracket
  | Power
  | Underscore
  | Backtick
  | LeftCurlyBracket
  | VerticalBar
  | RightCurlyBracket
  | Tilde

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
    'd' ->
      Just Dd
    'e' ->
      Just Ee
    'f' ->
      Just Ff
    'g' ->
      Just Gg
    'h' ->
      Just Hh
    'i' ->
      Just Ii
    'j' ->
      Just Jj
    'k' ->
      Just Kk
    'l' ->
      Just Ll
    'm' ->
      Just Mm
    'n' ->
      Just Nn
    'o' ->
      Just Oo
    'p' ->
      Just Pp
    'q' ->
      Just Qq
    'r' ->
      Just Rr
    's' ->
      Just Ss
    't' ->
      Just Tt
    'u' ->
      Just Uu
    'v' ->
      Just Vv
    'w' ->
      Just Ww
    'x' ->
      Just Xx
    'y' ->
      Just Yy
    'z' ->
      Just Zz
    'A' ->
      Just AA
    'B' ->
      Just BB
    'C' ->
      Just CC
    'D' ->
      Just DD
    'E' ->
      Just EE
    'F' ->
      Just FF
    'G' ->
      Just GG
    'H' ->
      Just HH
    'I' ->
      Just II
    'J' ->
      Just JJ
    'K' ->
      Just KK
    'L' ->
      Just LL
    'M' ->
      Just MM
    'N' ->
      Just NN
    'O' ->
      Just OO
    'P' ->
      Just PP
    'Q' ->
      Just QQ
    'R' ->
      Just RR
    'S' ->
      Just SS
    'T' ->
      Just TT
    'U' ->
      Just UU
    'V' ->
      Just VV
    'W' ->
      Just WW
    'X' ->
      Just XX
    'Y' ->
      Just YY
    'Z' ->
      Just ZZ
    '1' ->
      Just One
    '2' ->
      Just Two
    '3' ->
      Just Three
    '4' ->
      Just Four
    '5' ->
      Just Five
    '6' ->
      Just Six
    '7' ->
      Just Seven
    '8' ->
      Just Eight
    '9' ->
      Just Nine
    '0' ->
      Just Zero
    ' ' ->
      Just Space
    '!' ->
      Just ExclamationMark
    '#' ->
      Just Hash
    '$' ->
      Just Dollar
    '%' ->
      Just Percent
    '&' ->
      Just Ampersand
    '\'' ->
      Just Apostrophe
    '(' ->
      Just LeftParenthesis
    ')' ->
      Just RightParenthesis
    '*' ->
      Just Asterisk
    '+' ->
      Just Plus
    ',' ->
      Just Comma
    '-' ->
      Just Minus
    '.' ->
      Just Dot
    '/' ->
      Just Slash
    ':' ->
      Just Colon
    ';' ->
      Just Semicolon
    '<' ->
      Just LessThan
    '=' ->
      Just Equal
    '>' ->
      Just GreaterThan
    '?' ->
      Just QuestionMark
    '@' ->
      Just At
    '[' ->
      Just LeftSquareBracket
    ']' ->
      Just RightSquareBracket
    '^' ->
      Just Power
    '_' ->
      Just Underscore
    '`' ->
      Just Backtick
    '{' ->
      Just LeftCurlyBracket
    '|' ->
      Just VerticalBar
    '}' ->
      Just RightCurlyBracket
    '~' ->
      Just Tilde
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
    Dd ->
      "d"
    Ee ->
      "e"
    Ff ->
      "f"
    Gg ->
      "g"
    Hh ->
      "h"
    Ii ->
      "i"
    Jj ->
      "j"
    Kk ->
      "k"
    Ll ->
      "l"
    Mm ->
      "m"
    Nn ->
      "n"
    Oo ->
      "o"
    Pp ->
      "p"
    Qq ->
      "q"
    Rr ->
      "r"
    Ss ->
      "s"
    Tt ->
      "t"
    Uu ->
      "u"
    Vv ->
      "v"
    Ww ->
      "w"
    Xx ->
      "x"
    Yy ->
      "y"
    Zz ->
      "z"
    AA ->
      "A"
    BB ->
      "B"
    CC ->
      "C"
    DD ->
      "D"
    EE ->
      "E"
    FF ->
      "F"
    GG ->
      "G"
    HH ->
      "H"
    II ->
      "I"
    JJ ->
      "J"
    KK ->
      "K"
    LL ->
      "L"
    MM ->
      "M"
    NN ->
      "N"
    OO ->
      "O"
    PP ->
      "P"
    QQ ->
      "Q"
    RR ->
      "R"
    SS ->
      "S"
    TT ->
      "T"
    UU ->
      "U"
    VV ->
      "V"
    WW ->
      "W"
    XX ->
      "X"
    YY ->
      "Y"
    ZZ ->
      "Z"
    One ->
      "1"
    Two ->
      "2"
    Three ->
      "3"
    Four ->
      "4"
    Five ->
      "5"
    Six ->
      "6"
    Seven ->
      "7"
    Eight ->
      "8"
    Nine ->
      "9"
    Zero ->
      "0"
    Space ->
      " "
    ExclamationMark ->
      "!"
    Hash ->
      "#"
    Dollar ->
      "$"
    Percent ->
      "%"
    Ampersand ->
      "&"
    Apostrophe ->
      "'"
    LeftParenthesis ->
      "("
    RightParenthesis ->
      ")"
    Asterisk ->
      "*"
    Plus ->
      "+"
    Comma ->
      ","
    Minus ->
      "-"
    Dot ->
      "."
    Slash ->
      "/"
    Colon ->
      ":"
    Semicolon ->
      ";"
    LessThan ->
      "<"
    Equal ->
      "="
    GreaterThan ->
      ">"
    QuestionMark ->
      "?"
    At ->
      "@"
    LeftSquareBracket ->
      "["
    RightSquareBracket ->
      "]"
    Power ->
      "^"
    Underscore ->
      "_"
    Backtick ->
      "`"
    LeftCurlyBracket ->
      "{"
    VerticalBar ->
      "|"
    RightCurlyBracket ->
      "}"
    Tilde ->
      "~"
