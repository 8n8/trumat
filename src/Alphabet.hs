module Alphabet (Alphabet(..), fromChar, toChar) where


fromChar :: Char -> Maybe Alphabet
fromChar char =
    case char of
      'a' -> Just Ca
      'b' -> Just Cb
      'c' -> Just Cc
      'd' -> Just Cd
      'e' -> Just Ce
      'f' -> Just Cf
      'g' -> Just Cg
      'h' -> Just Ch
      'i' -> Just Ci
      'j' -> Just Cj
      'k' -> Just Ck
      'l' -> Just Cl
      'm' -> Just Cm
      'n' -> Just Cn
      'o' -> Just Co
      'p' -> Just Cp
      'q' -> Just Cq
      'r' -> Just Cr
      's' -> Just Cs
      't' -> Just Ct
      'u' -> Just Cu
      'v' -> Just Cv
      'w' -> Just Cw
      'x' -> Just Cx
      'y' -> Just Cy
      'z' -> Just Cz
      'A' -> Just CA
      'B' -> Just CB
      'C' -> Just CC
      'D' -> Just CD
      'E' -> Just CE
      'F' -> Just CF
      'G' -> Just CG
      'H' -> Just CH
      'I' -> Just CI
      'J' -> Just CJ
      'K' -> Just CK
      'L' -> Just CL
      'M' -> Just CM
      'N' -> Just CN
      'O' -> Just CO
      'P' -> Just CP
      'Q' -> Just CQ
      'R' -> Just CR
      'S' -> Just CS
      'T' -> Just CT
      'U' -> Just CU
      'V' -> Just CV
      'W' -> Just CW
      'X' -> Just CX
      'Y' -> Just CY
      'Z' -> Just CZ
      _ -> Nothing


toChar :: Alphabet -> Char
toChar alphabet =
    case alphabet of
      Ca -> 'a'
      Cb -> 'b'
      Cc -> 'c'
      Cd -> 'd'
      Ce -> 'e'
      Cf -> 'f'
      Cg -> 'g'
      Ch -> 'h'
      Ci -> 'i'
      Cj -> 'j'
      Ck -> 'k'
      Cl -> 'l'
      Cm -> 'm'
      Cn -> 'n'
      Co -> 'o'
      Cp -> 'p'
      Cq -> 'q'
      Cr -> 'r'
      Cs -> 's'
      Ct -> 't'
      Cu -> 'u'
      Cv -> 'v'
      Cw -> 'w'
      Cx -> 'x'
      Cy -> 'y'
      Cz -> 'z'
      CA -> 'A'
      CB -> 'B'
      CC -> 'C'
      CD -> 'D'
      CE -> 'E'
      CF -> 'F'
      CG -> 'G'
      CH -> 'H'
      CI -> 'I'
      CJ -> 'J'
      CK -> 'K'
      CL -> 'L'
      CM -> 'M'
      CN -> 'N'
      CO -> 'O'
      CP -> 'P'
      CQ -> 'Q'
      CR -> 'R'
      CS -> 'S'
      CT -> 'T'
      CU -> 'U'
      CV -> 'V'
      CW -> 'W'
      CX -> 'X'
      CY -> 'Y'
      CZ -> 'Z'

data Alphabet
    = Ca
    | Cb
    | Cc
    | Cd
    | Ce
    | Cf
    | Cg
    | Ch
    | Ci
    | Cj
    | Ck
    | Cl
    | Cm
    | Cn
    | Co
    | Cp
    | Cq
    | Cr
    | Cs
    | Ct
    | Cu
    | Cv
    | Cw
    | Cx
    | Cy
    | Cz
    | CA
    | CB
    | CC
    | CD
    | CE
    | CF
    | CG
    | CH
    | CI
    | CJ
    | CK
    | CL
    | CM
    | CN
    | CO
    | CP
    | CQ
    | CR
    | CS
    | CT
    | CU
    | CV
    | CW
    | CX
    | CY
    | CZ
