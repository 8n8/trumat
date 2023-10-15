module Digit (Digit(..), parse) where

import qualified ElmChar
import ElmChar (ElmChar)
import Prelude (Maybe(..))

data Digit
  = D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9


parse :: ElmChar -> Maybe Digit
parse char =
  case char of
    ElmChar.J_ ->
      Nothing
    ElmChar.H_ ->
      Nothing
    ElmChar.G_ ->
      Nothing
    ElmChar.F_ ->
      Nothing
    ElmChar.E_ ->
      Nothing
    ElmChar.D_ ->
      Nothing
    ElmChar.C_ ->
      Nothing
    ElmChar.Backtick ->
      Nothing
    ElmChar.Underscore ->
      Nothing
    ElmChar.Power ->
      Nothing
    ElmChar.CloseBracket ->
      Nothing
    ElmChar.Backslash ->
      Nothing
    ElmChar.OpenBracket ->
      Nothing
    ElmChar.Z' ->
      Nothing
    ElmChar.Y' ->
      Nothing
    ElmChar.X' ->
      Nothing
    ElmChar.W' ->
      Nothing
    ElmChar.V' ->
      Nothing
    ElmChar.U' ->
      Nothing
    ElmChar.T' ->
      Nothing
    ElmChar.S' ->
      Nothing
    ElmChar.R' ->
      Nothing
    ElmChar.Q' ->
      Nothing
    ElmChar.P' ->
      Nothing
    ElmChar.O' ->
      Nothing
    ElmChar.N' ->
      Nothing
    ElmChar.M' ->
      Nothing
    ElmChar.L' ->
      Nothing
    ElmChar.K' ->
      Nothing
    ElmChar.J' ->
      Nothing
    ElmChar.I' ->
      Nothing
    ElmChar.H' ->
      Nothing
    ElmChar.G' ->
      Nothing
    ElmChar.F' ->
      Nothing
    ElmChar.E' ->
      Nothing
    ElmChar.D' ->
      Nothing
    ElmChar.C' ->
      Nothing
    ElmChar.B' ->
      Nothing
    ElmChar.A' ->
      Nothing
    ElmChar.GreaterThan ->
      Nothing
    ElmChar.Equals ->
      Nothing
    ElmChar.LessThan ->
      Nothing
    ElmChar.Colon ->
      Nothing
    ElmChar.Nine ->
      Just D9
    ElmChar.Eight ->
      Just D8
    ElmChar.Seven ->
      Just D7
    ElmChar.Six ->
      Just D6
    ElmChar.Five ->
      Just D5
    ElmChar.Four ->
      Just D4
    ElmChar.Three ->
      Just D3
    ElmChar.Two ->
      Just D2
    ElmChar.One ->
      Just D1
    ElmChar.Zero ->
      Just D0
    ElmChar.ForwardSlash ->
      Nothing
    ElmChar.Dot ->
      Nothing
    ElmChar.Hyphen ->
      Nothing
    ElmChar.Comma ->
      Nothing
    ElmChar.Plus ->
      Nothing
    ElmChar.Asterisk ->
      Nothing
    ElmChar.CloseParentheses ->
      Nothing
    ElmChar.OpenParentheses ->
      Nothing
    ElmChar.SingleQuote ->
      Nothing
    ElmChar.DoubleQuote ->
      Nothing
    ElmChar.Space ->
      Nothing
    ElmChar.Newline ->
      Nothing
    ElmChar.I_ ->
      Nothing
    ElmChar.K_ ->
      Nothing
    ElmChar.L_ ->
      Nothing
    ElmChar.M_ ->
      Nothing
    ElmChar.N_ ->
      Nothing
    ElmChar.O_ ->
      Nothing
    ElmChar.P_ ->
      Nothing
    ElmChar.Q_ ->
      Nothing
    ElmChar.R_ ->
      Nothing
    ElmChar.S_ ->
      Nothing
    ElmChar.T_ ->
      Nothing
    ElmChar.U_ ->
      Nothing
    ElmChar.V_ ->
      Nothing
    ElmChar.W_ ->
      Nothing
    ElmChar.X_ ->
      Nothing
    ElmChar.Y_ ->
      Nothing
    ElmChar.Z_ ->
      Nothing
    ElmChar.OpenCurly ->
      Nothing
    ElmChar.Pipe ->
      Nothing
    ElmChar.CloseCurly ->
      Nothing
    ElmChar.OtherNonSpace ->
      Nothing
    ElmChar.A_ ->
      Nothing
    ElmChar.B_ ->
      Nothing
