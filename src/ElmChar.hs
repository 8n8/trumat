module ElmChar (ElmChar (..), parse, encode) where

import Data.Word (Word8)

data ElmChar
  = M_
  | O_
  | D_
  | U_
  | L_
  | E_
  | Space
  | X'
  | X_
  | P_
  | S_
  | I_
  | N_
  | G_
  | OpenParentheses
  | CloseParentheses
  | Newline
  | Equals
  | Zero
  deriving (Show)

encode :: ElmChar -> Word8
encode char =
  case char of
    M_ ->
      109
    O_ ->
      111
    D_ ->
      100
    U_ ->
      117
    L_ ->
      108
    E_ ->
      101
    Space ->
      32
    X' ->
      88
    X_ ->
      120
    P_ ->
      112
    S_ ->
      115
    I_ ->
      105
    N_ ->
      110
    G_ ->
      103
    OpenParentheses ->
      40
    CloseParentheses ->
      41
    Newline ->
      10
    Equals ->
      61
    Zero ->
      48

parse :: Word8 -> Either String ElmChar
parse word =
  case word of
    109 ->
      Right M_
    111 ->
      Right O_
    100 ->
      Right D_
    117 ->
      Right U_
    108 ->
      Right L_
    101 ->
      Right E_
    32 ->
      Right Space
    88 ->
      Right X'
    120 ->
      Right X_
    112 ->
      Right P_
    115 ->
      Right S_
    105 ->
      Right I_
    110 ->
      Right N_
    103 ->
      Right G_
    40 ->
      Right OpenParentheses
    41 ->
      Right CloseParentheses
    10 ->
      Right Newline
    61 ->
      Right Equals
    48 ->
      Right Zero
    _ ->
      Left ("invalid Elm character: " <> show word)
