module UpperChar (UpperChar(..), parse) where

import Prelude (Maybe(..))
import ElmChar (ElmChar)
import qualified ElmChar

parse :: ElmChar -> Maybe UpperChar
parse char =
  case char of
    ElmChar.A' -> Just A
    ElmChar.B' -> Just B
    ElmChar.C' -> Just C
    ElmChar.D' -> Just D
    ElmChar.E' -> Just E
    ElmChar.F' -> Just F
    ElmChar.G' -> Just G
    ElmChar.H' -> Just H
    ElmChar.I' -> Just I
    ElmChar.J' -> Just J
    ElmChar.K' -> Just K
    ElmChar.L' -> Just L
    ElmChar.M' -> Just M
    ElmChar.N' -> Just N
    ElmChar.O' -> Just O
    ElmChar.P' -> Just P
    ElmChar.Q' -> Just Q
    ElmChar.R' -> Just R
    ElmChar.S' -> Just S
    ElmChar.T' -> Just T
    ElmChar.U' -> Just U
    ElmChar.V' -> Just V
    ElmChar.W' -> Just W
    ElmChar.X' -> Just X
    ElmChar.Y' -> Just Y
    ElmChar.Z' -> Just Z
    ElmChar.A_ -> Nothing
    ElmChar.B_ -> Nothing
    ElmChar.C_ -> Nothing
    ElmChar.D_ -> Nothing
    ElmChar.E_ -> Nothing
    ElmChar.F_ -> Nothing
    ElmChar.G_ -> Nothing
    ElmChar.H_ -> Nothing
    ElmChar.I_ -> Nothing
    ElmChar.J_ -> Nothing
    ElmChar.K_ -> Nothing
    ElmChar.L_ -> Nothing
    ElmChar.M_ -> Nothing
    ElmChar.N_ -> Nothing
    ElmChar.O_ -> Nothing
    ElmChar.P_ -> Nothing
    ElmChar.Q_ -> Nothing
    ElmChar.R_ -> Nothing
    ElmChar.S_ -> Nothing
    ElmChar.T_ -> Nothing
    ElmChar.U_ -> Nothing
    ElmChar.V_ -> Nothing
    ElmChar.W_ -> Nothing
    ElmChar.X_ -> Nothing
    ElmChar.Y_ -> Nothing
    ElmChar.Z_ -> Nothing
    ElmChar.OtherNonSpace -> Nothing
    ElmChar.CloseCurly -> Nothing
    ElmChar.Pipe -> Nothing
    ElmChar.OpenCurly -> Nothing
    ElmChar.Backtick -> Nothing
    ElmChar.Underscore -> Nothing
    ElmChar.Power -> Nothing
    ElmChar.CloseBracket -> Nothing
    ElmChar.Backslash -> Nothing
    ElmChar.OpenBracket -> Nothing
    ElmChar.GreaterThan -> Nothing
    ElmChar.Equals -> Nothing
    ElmChar.LessThan -> Nothing
    ElmChar.Colon -> Nothing
    ElmChar.Nine -> Nothing
    ElmChar.Eight -> Nothing
    ElmChar.Seven -> Nothing
    ElmChar.Six -> Nothing
    ElmChar.Five -> Nothing
    ElmChar.Four -> Nothing
    ElmChar.Three -> Nothing
    ElmChar.Two -> Nothing
    ElmChar.One -> Nothing
    ElmChar.Zero -> Nothing
    ElmChar.ForwardSlash -> Nothing
    ElmChar.Dot -> Nothing
    ElmChar.Hyphen -> Nothing
    ElmChar.Comma -> Nothing
    ElmChar.Plus -> Nothing
    ElmChar.Asterisk -> Nothing
    ElmChar.CloseParentheses -> Nothing
    ElmChar.OpenParentheses -> Nothing
    ElmChar.SingleQuote -> Nothing
    ElmChar.DoubleQuote -> Nothing
    ElmChar.Space -> Nothing
    ElmChar.Newline -> Nothing

data UpperChar
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
