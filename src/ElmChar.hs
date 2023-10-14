module ElmChar (ElmChar, toByte, parse) where

import Data.Word (Word8)
import Prelude (Maybe (..))

data ElmChar
  = A_
  | OtherNonSpace
  | CloseCurly
  | Pipe
  | OpenCurly
  | Z_
  | Y_
  | X_
  | W_
  | V_
  | U_
  | T_
  | S_
  | R_
  | Q_
  | P_
  | O_
  | N_
  | M_
  | L_
  | K_
  | I_
  | J_
  | H_
  | G_
  | F_
  | E_
  | D_
  | C_
  | B_
  | Backtick
  | Underscore
  | Power
  | CloseBracket
  | Backslash
  | OpenBracket
  | Z'
  | Y'
  | X'
  | W'
  | V'
  | U'
  | T'
  | S'
  | R'
  | Q'
  | P'
  | O'
  | N'
  | M'
  | L'
  | K'
  | J'
  | I'
  | H'
  | G'
  | F'
  | E'
  | D'
  | C'
  | B'
  | A'
  | GreaterThan
  | Equals
  | LessThan
  | Colon
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  | One
  | Zero
  | ForwardSlash
  | Dot
  | Hyphen
  | Comma
  | Plus
  | Asterisk
  | CloseParentheses
  | OpenParentheses
  | SingleQuote
  | DoubleQuote
  | Space
  | Newline

toByte :: ElmChar -> Word8
toByte char =
  case char of
    Newline -> 10
    Space -> 32
    DoubleQuote -> 34
    SingleQuote -> 39
    OpenParentheses -> 40
    CloseParentheses -> 41
    Asterisk -> 42
    Plus -> 43
    Comma -> 44
    Hyphen -> 45
    Dot -> 46
    ForwardSlash -> 47
    Zero -> 48
    One -> 49
    Two -> 50
    Three -> 51
    Four -> 52
    Five -> 53
    Six -> 54
    Seven -> 55
    Eight -> 56
    Nine -> 57
    Colon -> 58
    LessThan -> 60
    Equals -> 61
    GreaterThan -> 62
    A' -> 65
    B' -> 66
    C' -> 67
    D' -> 68
    E' -> 69
    F' -> 70
    G' -> 71
    H' -> 72
    I' -> 73
    J' -> 74
    K' -> 75
    L' -> 76
    M' -> 77
    N' -> 78
    O' -> 79
    P' -> 80
    Q' -> 81
    R' -> 82
    S' -> 83
    T' -> 84
    U' -> 85
    V' -> 86
    W' -> 87
    X' -> 88
    Y' -> 89
    Z' -> 90
    OpenBracket -> 91
    Backslash -> 92
    CloseBracket -> 93
    Power -> 94
    Underscore -> 95
    Backtick -> 96
    A_ -> 97
    B_ -> 98
    C_ -> 99
    D_ -> 100
    E_ -> 101
    F_ -> 102
    G_ -> 103
    H_ -> 104
    I_ -> 105
    J_ -> 106
    K_ -> 107
    L_ -> 108
    M_ -> 109
    N_ -> 110
    O_ -> 111
    P_ -> 112
    Q_ -> 113
    R_ -> 114
    S_ -> 115
    T_ -> 116
    U_ -> 117
    V_ -> 118
    W_ -> 119
    X_ -> 120
    Y_ -> 121
    Z_ -> 122
    OpenCurly -> 123
    Pipe -> 124
    CloseCurly -> 125
    OtherNonSpace -> 128

parse :: Word8 -> Maybe ElmChar
parse byte =
  case byte of
    10 -> Just Newline
    32 -> Just Space
    33 -> Just OtherNonSpace
    34 -> Just DoubleQuote
    35 -> Just OtherNonSpace
    36 -> Just OtherNonSpace
    37 -> Just OtherNonSpace
    38 -> Just OtherNonSpace
    39 -> Just SingleQuote
    40 -> Just OpenParentheses
    41 -> Just CloseParentheses
    42 -> Just Asterisk
    43 -> Just Plus
    44 -> Just Comma
    45 -> Just Hyphen
    46 -> Just Dot
    47 -> Just ForwardSlash
    48 -> Just Zero
    49 -> Just One
    50 -> Just Two
    51 -> Just Three
    52 -> Just Four
    53 -> Just Five
    54 -> Just Six
    55 -> Just Seven
    56 -> Just Eight
    57 -> Just Nine
    58 -> Just Colon
    59 -> Just OtherNonSpace
    60 -> Just LessThan
    61 -> Just Equals
    62 -> Just GreaterThan
    63 -> Just OtherNonSpace
    64 -> Just OtherNonSpace
    65 -> Just A'
    66 -> Just B'
    67 -> Just C'
    68 -> Just D'
    69 -> Just E'
    70 -> Just F'
    71 -> Just G'
    72 -> Just H'
    73 -> Just I'
    74 -> Just J'
    75 -> Just K'
    76 -> Just L'
    77 -> Just M'
    78 -> Just N'
    79 -> Just O'
    80 -> Just P'
    81 -> Just Q'
    82 -> Just R'
    83 -> Just S'
    84 -> Just T'
    85 -> Just U'
    86 -> Just V'
    87 -> Just W'
    88 -> Just X'
    89 -> Just Y'
    90 -> Just Z'
    91 -> Just OpenBracket
    92 -> Just Backslash
    93 -> Just CloseBracket
    94 -> Just Power
    95 -> Just Underscore
    96 -> Just Backtick
    97 -> Just A_
    98 -> Just B_
    99 -> Just C_
    100 -> Just D_
    101 -> Just E_
    102 -> Just F_
    103 -> Just G_
    104 -> Just H_
    105 -> Just I_
    106 -> Just J_
    107 -> Just K_
    108 -> Just L_
    109 -> Just M_
    110 -> Just N_
    111 -> Just O_
    112 -> Just P_
    113 -> Just Q_
    114 -> Just R_
    115 -> Just S_
    116 -> Just T_
    117 -> Just U_
    118 -> Just V_
    119 -> Just W_
    120 -> Just X_
    121 -> Just Y_
    122 -> Just Z_
    123 -> Just OpenCurly
    124 -> Just Pipe
    125 -> Just CloseCurly
    126 -> Just OtherNonSpace
    128 -> Just OtherNonSpace
    129 -> Just OtherNonSpace
    130 -> Just OtherNonSpace
    131 -> Just OtherNonSpace
    132 -> Just OtherNonSpace
    133 -> Just OtherNonSpace
    134 -> Just OtherNonSpace
    135 -> Just OtherNonSpace
    136 -> Just OtherNonSpace
    137 -> Just OtherNonSpace
    138 -> Just OtherNonSpace
    139 -> Just OtherNonSpace
    140 -> Just OtherNonSpace
    141 -> Just OtherNonSpace
    142 -> Just OtherNonSpace
    143 -> Just OtherNonSpace
    144 -> Just OtherNonSpace
    145 -> Just OtherNonSpace
    146 -> Just OtherNonSpace
    147 -> Just OtherNonSpace
    148 -> Just OtherNonSpace
    149 -> Just OtherNonSpace
    150 -> Just OtherNonSpace
    151 -> Just OtherNonSpace
    152 -> Just OtherNonSpace
    153 -> Just OtherNonSpace
    154 -> Just OtherNonSpace
    155 -> Just OtherNonSpace
    156 -> Just OtherNonSpace
    157 -> Just OtherNonSpace
    158 -> Just OtherNonSpace
    159 -> Just OtherNonSpace
    160 -> Just OtherNonSpace
    161 -> Just OtherNonSpace
    162 -> Just OtherNonSpace
    163 -> Just OtherNonSpace
    164 -> Just OtherNonSpace
    165 -> Just OtherNonSpace
    166 -> Just OtherNonSpace
    167 -> Just OtherNonSpace
    168 -> Just OtherNonSpace
    169 -> Just OtherNonSpace
    170 -> Just OtherNonSpace
    171 -> Just OtherNonSpace
    172 -> Just OtherNonSpace
    173 -> Just OtherNonSpace
    174 -> Just OtherNonSpace
    175 -> Just OtherNonSpace
    176 -> Just OtherNonSpace
    177 -> Just OtherNonSpace
    178 -> Just OtherNonSpace
    179 -> Just OtherNonSpace
    180 -> Just OtherNonSpace
    181 -> Just OtherNonSpace
    182 -> Just OtherNonSpace
    183 -> Just OtherNonSpace
    184 -> Just OtherNonSpace
    185 -> Just OtherNonSpace
    186 -> Just OtherNonSpace
    187 -> Just OtherNonSpace
    188 -> Just OtherNonSpace
    189 -> Just OtherNonSpace
    190 -> Just OtherNonSpace
    191 -> Just OtherNonSpace
    192 -> Just OtherNonSpace
    193 -> Just OtherNonSpace
    194 -> Just OtherNonSpace
    195 -> Just OtherNonSpace
    196 -> Just OtherNonSpace
    197 -> Just OtherNonSpace
    198 -> Just OtherNonSpace
    199 -> Just OtherNonSpace
    200 -> Just OtherNonSpace
    201 -> Just OtherNonSpace
    202 -> Just OtherNonSpace
    203 -> Just OtherNonSpace
    204 -> Just OtherNonSpace
    205 -> Just OtherNonSpace
    206 -> Just OtherNonSpace
    207 -> Just OtherNonSpace
    208 -> Just OtherNonSpace
    209 -> Just OtherNonSpace
    210 -> Just OtherNonSpace
    211 -> Just OtherNonSpace
    212 -> Just OtherNonSpace
    213 -> Just OtherNonSpace
    214 -> Just OtherNonSpace
    215 -> Just OtherNonSpace
    216 -> Just OtherNonSpace
    217 -> Just OtherNonSpace
    218 -> Just OtherNonSpace
    219 -> Just OtherNonSpace
    220 -> Just OtherNonSpace
    221 -> Just OtherNonSpace
    222 -> Just OtherNonSpace
    223 -> Just OtherNonSpace
    224 -> Just OtherNonSpace
    225 -> Just OtherNonSpace
    226 -> Just OtherNonSpace
    227 -> Just OtherNonSpace
    228 -> Just OtherNonSpace
    229 -> Just OtherNonSpace
    230 -> Just OtherNonSpace
    231 -> Just OtherNonSpace
    232 -> Just OtherNonSpace
    233 -> Just OtherNonSpace
    234 -> Just OtherNonSpace
    235 -> Just OtherNonSpace
    236 -> Just OtherNonSpace
    237 -> Just OtherNonSpace
    238 -> Just OtherNonSpace
    239 -> Just OtherNonSpace
    240 -> Just OtherNonSpace
    241 -> Just OtherNonSpace
    242 -> Just OtherNonSpace
    243 -> Just OtherNonSpace
    244 -> Just OtherNonSpace
    245 -> Just OtherNonSpace
    246 -> Just OtherNonSpace
    247 -> Just OtherNonSpace
    248 -> Just OtherNonSpace
    249 -> Just OtherNonSpace
    250 -> Just OtherNonSpace
    251 -> Just OtherNonSpace
    252 -> Just OtherNonSpace
    253 -> Just OtherNonSpace
    254 -> Just OtherNonSpace
    255 -> Just OtherNonSpace
    _ -> Nothing
