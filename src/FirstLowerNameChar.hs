module FirstLowerNameChar (FirstLowerNameChar, parse) where

import ElmChar (ElmChar)
import qualified ElmChar
import Prelude (Maybe(..))

parse :: ElmChar -> Maybe FirstLowerNameChar
parse char =
  case char of
    ElmChar.A_ -> Just A
    ElmChar.B_ -> Just B
    ElmChar.C_ -> Just C
    ElmChar.D_ -> Just D
    ElmChar.E_ -> Just E
    ElmChar.F_ -> Just F
    ElmChar.G_ -> Just G
    ElmChar.H_ -> Just H
    ElmChar.I_ -> Just I
    ElmChar.J_ -> Just J
    ElmChar.K_ -> Just K
    ElmChar.L_ -> Just L
    ElmChar.M_ -> Just M
    ElmChar.N_ -> Just N
    ElmChar.O_ -> Just O
    ElmChar.P_ -> Just P
    ElmChar.Q_ -> Just Q
    ElmChar.R_ -> Just R
    ElmChar.S_ -> Just S
    ElmChar.T_ -> Just T
    ElmChar.V_ -> Just V
    ElmChar.W_ -> Just W
    ElmChar.X_ -> Just X
    ElmChar.Y_ -> Just Y
    ElmChar.Z_ -> Just Z
    ElmChar.Underscore -> Just Underscore
    ElmChar.OtherNonSpace -> Nothing
    ElmChar.CloseCurly -> Nothing
    ElmChar.Pipe -> Nothing
    ElmChar.OpenCurly -> Nothing
    ElmChar.U_ -> Just U
    ElmChar.Backtick -> Nothing
    ElmChar.Power -> Nothing
    ElmChar.CloseBracket -> Nothing
    ElmChar.Backslash -> Nothing
    ElmChar.OpenBracket -> Nothing
    ElmChar.Z' -> Nothing
    ElmChar.Y' -> Nothing
    ElmChar.X' -> Nothing
    ElmChar.W' -> Nothing
    ElmChar.V' -> Nothing
    ElmChar.U' -> Nothing
    ElmChar.T' -> Nothing
    ElmChar.S' -> Nothing
    ElmChar.R' -> Nothing
    ElmChar.Q' -> Nothing
    ElmChar.P' -> Nothing
    ElmChar.O' -> Nothing
    ElmChar.N' -> Nothing
    ElmChar.M' -> Nothing
    ElmChar.L' -> Nothing
    ElmChar.K' -> Nothing
    ElmChar.J' -> Nothing
    ElmChar.I' -> Nothing
    ElmChar.H' -> Nothing
    ElmChar.G' -> Nothing
    ElmChar.F' -> Nothing
    ElmChar.E' -> Nothing
    ElmChar.D' -> Nothing
    ElmChar.C' -> Nothing
    ElmChar.B' -> Nothing
    ElmChar.A' -> Nothing
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

data FirstLowerNameChar
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
  | Underscore
