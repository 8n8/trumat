module SubsequentNameChar (SubsequentNameChar(..), parse) where

import Prelude (Maybe(..))
import qualified ElmChar
import ElmChar (ElmChar)

parse :: ElmChar -> Maybe SubsequentNameChar
parse elmChar =
  case elmChar of
    ElmChar.A' -> Just A'
    ElmChar.A_ -> Just A_
    ElmChar.OtherNonSpace -> Nothing
    ElmChar.CloseCurly -> Nothing
    ElmChar.Pipe -> Nothing
    ElmChar.Z_ -> Just Z_
    ElmChar.OpenCurly -> Nothing
    ElmChar.Y_ -> Just Y_
    ElmChar.X_ -> Just X_
    ElmChar.W_ -> Just W_
    ElmChar.V_ -> Just V_
    ElmChar.U_ -> Just U_
    ElmChar.T_ -> Just T_
    ElmChar.S_ -> Just S_
    ElmChar.R_ -> Just R_
    ElmChar.Q_ -> Just Q_
    ElmChar.P_ -> Just P_
    ElmChar.O_ -> Just O_
    ElmChar.N_ -> Just N_
    ElmChar.M_ -> Just M_
    ElmChar.L_ -> Just L_
    ElmChar.K_ -> Just K_
    ElmChar.I_ -> Just I_
    ElmChar.J_ -> Just J_
    ElmChar.H_ -> Just H_
    ElmChar.F_ -> Just F_
    ElmChar.G_ -> Just G_
    ElmChar.E_ -> Just E_
    ElmChar.D_ -> Just D_
    ElmChar.C_ -> Just C_
    ElmChar.B_ -> Just B_
    ElmChar.Backtick -> Nothing
    ElmChar.Underscore -> Just Underscore
    ElmChar.Power -> Nothing
    ElmChar.CloseBracket -> Nothing
    ElmChar.Backslash -> Nothing
    ElmChar.OpenBracket -> Nothing
    ElmChar.Z' -> Just Z'
    ElmChar.Y' -> Just Y'
    ElmChar.X' -> Just X'
    ElmChar.W' -> Just W'
    ElmChar.V' -> Just V'
    ElmChar.U' -> Just U'
    ElmChar.T' -> Just T'
    ElmChar.S' -> Just S'
    ElmChar.R' -> Just R'
    ElmChar.Q' -> Just Q'
    ElmChar.P' -> Just P'
    ElmChar.O' -> Just O'
    ElmChar.N' -> Just N'
    ElmChar.M' -> Just M'
    ElmChar.L' -> Just L'
    ElmChar.K' -> Just K'
    ElmChar.J' -> Just J'
    ElmChar.I' -> Just I'
    ElmChar.H' -> Just H'
    ElmChar.G' -> Just G'
    ElmChar.F' -> Just F'
    ElmChar.E' -> Just E'
    ElmChar.D' -> Just D'
    ElmChar.C' -> Just C'
    ElmChar.B' -> Just B'
    ElmChar.GreaterThan -> Nothing
    ElmChar.Equals -> Nothing
    ElmChar.LessThan -> Nothing
    ElmChar.Colon -> Nothing
    ElmChar.Nine -> Just Nine
    ElmChar.Eight -> Just Eight
    ElmChar.Seven -> Just Seven
    ElmChar.Six -> Just Six
    ElmChar.Five -> Just Five
    ElmChar.Four -> Just Four
    ElmChar.Three -> Just Three
    ElmChar.Two -> Just Two
    ElmChar.One -> Just One
    ElmChar.Zero -> Just Zero
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


data SubsequentNameChar
  = Underscore
  | A'
  | B'
  | C'
  | D'
  | E'
  | F'
  | G'
  | H'
  | I'
  | J'
  | K'
  | L'
  | M'
  | N'
  | O'
  | P'
  | Q'
  | R'
  | S'
  | T'
  | U'
  | V'
  | W'
  | X'
  | Y'
  | Z'
  | A_
  | B_
  | C_
  | D_
  | E_
  | F_
  | G_
  | H_
  | I_
  | J_
  | K_
  | L_
  | M_
  | N_
  | O_
  | P_
  | Q_
  | R_
  | S_
  | T_
  | U_
  | V_
  | W_
  | X_
  | Y_
  | Z_
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
