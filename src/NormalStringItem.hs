module NormalStringItem (NormalStringItem, parse, write) where

import qualified Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Word (Word8)

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
  | C128
  | C129
  | C130
  | C131
  | C132
  | C133
  | C134
  | C135
  | C136
  | C137
  | C138
  | C139
  | C140
  | C141
  | C142
  | C143
  | C144
  | C145
  | C146
  | C147
  | C148
  | C149
  | C150
  | C151
  | C152
  | C153
  | C154
  | C155
  | C156
  | C157
  | C158
  | C159
  | C160
  | C161
  | C162
  | C163
  | C164
  | C165
  | C166
  | C167
  | C168
  | C169
  | C170
  | C180
  | C181
  | C182
  | C183
  | C184
  | C185
  | C186
  | C187
  | C188
  | C189
  | C190
  | C191
  | C192
  | C193
  | C194
  | C195
  | C196
  | C197
  | C198
  | C199
  | C200
  | C201
  | C202
  | C203
  | C204
  | C205
  | C206
  | C207
  | C208
  | C209
  | C210
  | C211
  | C212
  | C213
  | C214
  | C215
  | C216
  | C217
  | C218
  | C219
  | C220
  | C221
  | C222
  | C223
  | C224
  | C225
  | C226
  | C227
  | C228
  | C229
  | C230
  | C231
  | C232
  | C233
  | C234
  | C235
  | C236
  | C237
  | C238
  | C239
  | C240
  | C241
  | C242
  | C243
  | C244
  | C245
  | C246
  | C247
  | C248
  | C249
  | C250
  | C251
  | C252
  | C253
  | C254
  | C255

parse :: Parser NormalStringItem
parse =
  do
    ch <- Data.Attoparsec.ByteString.anyWord8
    case wordToItem ch of
      Nothing ->
        fail "Invalid normal string item"
      Just item ->
        pure item

wordToItem :: Word8 -> Maybe NormalStringItem
wordToItem word =
  case word of
    32 ->
      Just Space
    33 ->
      Just ExclamationMark
    35 ->
      Just Hash
    36 ->
      Just Dollar
    37 ->
      Just Percent
    38 ->
      Just Ampersand
    39 ->
      Just Apostrophe
    40 ->
      Just LeftParenthesis
    41 ->
      Just RightParenthesis
    42 ->
      Just Asterisk
    43 ->
      Just Plus
    44 ->
      Just Comma
    45 ->
      Just Minus
    46 ->
      Just Dot
    47 ->
      Just Slash
    48 ->
      Just Zero
    49 ->
      Just One
    50 ->
      Just Two
    51 ->
      Just Three
    52 ->
      Just Four
    53 ->
      Just Five
    54 ->
      Just Six
    55 ->
      Just Seven
    56 ->
      Just Eight
    57 ->
      Just Nine
    58 ->
      Just Colon
    59 ->
      Just Semicolon
    60 ->
      Just LessThan
    61 ->
      Just Equal
    62 ->
      Just GreaterThan
    63 ->
      Just QuestionMark
    64 ->
      Just At
    65 ->
      Just AA
    66 ->
      Just BB
    67 ->
      Just CC
    68 ->
      Just DD
    69 ->
      Just EE
    70 ->
      Just FF
    71 ->
      Just GG
    72 ->
      Just HH
    73 ->
      Just II
    74 ->
      Just JJ
    75 ->
      Just KK
    76 ->
      Just LL
    77 ->
      Just MM
    78 ->
      Just NN
    79 ->
      Just OO
    80 ->
      Just PP
    81 ->
      Just QQ
    82 ->
      Just RR
    83 ->
      Just SS
    84 ->
      Just TT
    85 ->
      Just UU
    86 ->
      Just VV
    87 ->
      Just WW
    88 ->
      Just XX
    89 ->
      Just YY
    90 ->
      Just ZZ
    91 ->
      Just LeftSquareBracket
    93 ->
      Just RightSquareBracket
    94 ->
      Just Power
    95 ->
      Just Underscore
    96 ->
      Just Backtick
    97 ->
      Just Aa
    98 ->
      Just Bb
    99 ->
      Just Cc
    100 ->
      Just Dd
    101 ->
      Just Ee
    102 ->
      Just Ff
    103 ->
      Just Gg
    104 ->
      Just Hh
    105 ->
      Just Ii
    106 ->
      Just Jj
    107 ->
      Just Kk
    108 ->
      Just Ll
    109 ->
      Just Mm
    110 ->
      Just Nn
    111 ->
      Just Oo
    112 ->
      Just Pp
    113 ->
      Just Qq
    114 ->
      Just Rr
    115 ->
      Just Ss
    116 ->
      Just Tt
    117 ->
      Just Uu
    118 ->
      Just Vv
    119 ->
      Just Ww
    120 ->
      Just Xx
    121 ->
      Just Yy
    122 ->
      Just Zz
    123 ->
      Just LeftCurlyBracket
    124 ->
      Just VerticalBar
    125 ->
      Just RightCurlyBracket
    126 ->
      Just Tilde
    128 ->
      Just C128
    129 ->
      Just C129
    130 ->
      Just C130
    131 ->
      Just C131
    132 ->
      Just C132
    133 ->
      Just C133
    134 ->
      Just C134
    135 ->
      Just C135
    136 ->
      Just C136
    137 ->
      Just C137
    138 ->
      Just C138
    139 ->
      Just C139
    140 ->
      Just C140
    141 ->
      Just C141
    142 ->
      Just C142
    143 ->
      Just C143
    144 ->
      Just C144
    145 ->
      Just C145
    146 ->
      Just C146
    147 ->
      Just C147
    148 ->
      Just C148
    149 ->
      Just C149
    150 ->
      Just C150
    151 ->
      Just C151
    152 ->
      Just C152
    153 ->
      Just C153
    154 ->
      Just C154
    155 ->
      Just C155
    156 ->
      Just C156
    157 ->
      Just C157
    158 ->
      Just C158
    159 ->
      Just C159
    160 ->
      Just C160
    161 ->
      Just C161
    162 ->
      Just C162
    163 ->
      Just C163
    164 ->
      Just C164
    165 ->
      Just C165
    166 ->
      Just C166
    167 ->
      Just C167
    168 ->
      Just C168
    169 ->
      Just C169
    170 ->
      Just C170
    180 ->
      Just C180
    181 ->
      Just C181
    182 ->
      Just C182
    183 ->
      Just C183
    184 ->
      Just C184
    185 ->
      Just C185
    186 ->
      Just C186
    187 ->
      Just C187
    188 ->
      Just C188
    189 ->
      Just C189
    190 ->
      Just C190
    191 ->
      Just C191
    192 ->
      Just C192
    193 ->
      Just C193
    194 ->
      Just C194
    195 ->
      Just C195
    196 ->
      Just C196
    197 ->
      Just C197
    198 ->
      Just C198
    199 ->
      Just C199
    200 ->
      Just C200
    201 ->
      Just C201
    202 ->
      Just C202
    203 ->
      Just C203
    204 ->
      Just C204
    205 ->
      Just C205
    206 ->
      Just C206
    207 ->
      Just C207
    208 ->
      Just C208
    209 ->
      Just C209
    210 ->
      Just C210
    211 ->
      Just C211
    212 ->
      Just C212
    213 ->
      Just C213
    214 ->
      Just C214
    215 ->
      Just C215
    216 ->
      Just C216
    217 ->
      Just C217
    218 ->
      Just C218
    219 ->
      Just C219
    220 ->
      Just C220
    221 ->
      Just C221
    222 ->
      Just C222
    223 ->
      Just C223
    224 ->
      Just C224
    225 ->
      Just C225
    226 ->
      Just C226
    227 ->
      Just C227
    228 ->
      Just C228
    229 ->
      Just C229
    230 ->
      Just C230
    231 ->
      Just C231
    232 ->
      Just C232
    233 ->
      Just C233
    234 ->
      Just C234
    235 ->
      Just C235
    236 ->
      Just C236
    237 ->
      Just C237
    238 ->
      Just C238
    239 ->
      Just C239
    240 ->
      Just C240
    241 ->
      Just C241
    242 ->
      Just C242
    243 ->
      Just C243
    244 ->
      Just C244
    245 ->
      Just C245
    246 ->
      Just C246
    247 ->
      Just C247
    248 ->
      Just C248
    249 ->
      Just C249
    250 ->
      Just C250
    251 ->
      Just C251
    252 ->
      Just C252
    253 ->
      Just C253
    254 ->
      Just C254
    255 ->
      Just C255
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
    C128 ->
      Data.ByteString.singleton 128
    C129 ->
      Data.ByteString.singleton 129
    C130 ->
      Data.ByteString.singleton 130
    C131 ->
      Data.ByteString.singleton 131
    C132 ->
      Data.ByteString.singleton 132
    C133 ->
      Data.ByteString.singleton 133
    C134 ->
      Data.ByteString.singleton 134
    C135 ->
      Data.ByteString.singleton 135
    C136 ->
      Data.ByteString.singleton 136
    C137 ->
      Data.ByteString.singleton 137
    C138 ->
      Data.ByteString.singleton 138
    C139 ->
      Data.ByteString.singleton 139
    C140 ->
      Data.ByteString.singleton 140
    C141 ->
      Data.ByteString.singleton 141
    C142 ->
      Data.ByteString.singleton 142
    C143 ->
      Data.ByteString.singleton 143
    C144 ->
      Data.ByteString.singleton 144
    C145 ->
      Data.ByteString.singleton 145
    C146 ->
      Data.ByteString.singleton 146
    C147 ->
      Data.ByteString.singleton 147
    C148 ->
      Data.ByteString.singleton 148
    C149 ->
      Data.ByteString.singleton 149
    C150 ->
      Data.ByteString.singleton 150
    C151 ->
      Data.ByteString.singleton 151
    C152 ->
      Data.ByteString.singleton 152
    C153 ->
      Data.ByteString.singleton 153
    C154 ->
      Data.ByteString.singleton 154
    C155 ->
      Data.ByteString.singleton 155
    C156 ->
      Data.ByteString.singleton 156
    C157 ->
      Data.ByteString.singleton 157
    C158 ->
      Data.ByteString.singleton 158
    C159 ->
      Data.ByteString.singleton 159
    C160 ->
      Data.ByteString.singleton 160
    C161 ->
      Data.ByteString.singleton 161
    C162 ->
      Data.ByteString.singleton 162
    C163 ->
      Data.ByteString.singleton 163
    C164 ->
      Data.ByteString.singleton 164
    C165 ->
      Data.ByteString.singleton 165
    C166 ->
      Data.ByteString.singleton 166
    C167 ->
      Data.ByteString.singleton 167
    C168 ->
      Data.ByteString.singleton 168
    C169 ->
      Data.ByteString.singleton 169
    C170 ->
      Data.ByteString.singleton 170
    C180 ->
      Data.ByteString.singleton 180
    C181 ->
      Data.ByteString.singleton 181
    C182 ->
      Data.ByteString.singleton 182
    C183 ->
      Data.ByteString.singleton 183
    C184 ->
      Data.ByteString.singleton 184
    C185 ->
      Data.ByteString.singleton 185
    C186 ->
      Data.ByteString.singleton 186
    C187 ->
      Data.ByteString.singleton 187
    C188 ->
      Data.ByteString.singleton 188
    C189 ->
      Data.ByteString.singleton 189
    C190 ->
      Data.ByteString.singleton 190
    C191 ->
      Data.ByteString.singleton 191
    C192 ->
      Data.ByteString.singleton 192
    C193 ->
      Data.ByteString.singleton 193
    C194 ->
      Data.ByteString.singleton 194
    C195 ->
      Data.ByteString.singleton 195
    C196 ->
      Data.ByteString.singleton 196
    C197 ->
      Data.ByteString.singleton 197
    C198 ->
      Data.ByteString.singleton 198
    C199 ->
      Data.ByteString.singleton 199
    C200 ->
      Data.ByteString.singleton 200
    C201 ->
      Data.ByteString.singleton 201
    C202 ->
      Data.ByteString.singleton 202
    C203 ->
      Data.ByteString.singleton 203
    C204 ->
      Data.ByteString.singleton 204
    C205 ->
      Data.ByteString.singleton 205
    C206 ->
      Data.ByteString.singleton 206
    C207 ->
      Data.ByteString.singleton 207
    C208 ->
      Data.ByteString.singleton 208
    C209 ->
      Data.ByteString.singleton 209
    C210 ->
      Data.ByteString.singleton 210
    C211 ->
      Data.ByteString.singleton 211
    C212 ->
      Data.ByteString.singleton 212
    C213 ->
      Data.ByteString.singleton 213
    C214 ->
      Data.ByteString.singleton 214
    C215 ->
      Data.ByteString.singleton 215
    C216 ->
      Data.ByteString.singleton 216
    C217 ->
      Data.ByteString.singleton 217
    C218 ->
      Data.ByteString.singleton 218
    C219 ->
      Data.ByteString.singleton 219
    C220 ->
      Data.ByteString.singleton 220
    C221 ->
      Data.ByteString.singleton 221
    C222 ->
      Data.ByteString.singleton 222
    C223 ->
      Data.ByteString.singleton 223
    C224 ->
      Data.ByteString.singleton 224
    C225 ->
      Data.ByteString.singleton 225
    C226 ->
      Data.ByteString.singleton 226
    C227 ->
      Data.ByteString.singleton 227
    C228 ->
      Data.ByteString.singleton 228
    C229 ->
      Data.ByteString.singleton 229
    C230 ->
      Data.ByteString.singleton 230
    C231 ->
      Data.ByteString.singleton 231
    C232 ->
      Data.ByteString.singleton 232
    C233 ->
      Data.ByteString.singleton 233
    C234 ->
      Data.ByteString.singleton 234
    C235 ->
      Data.ByteString.singleton 235
    C236 ->
      Data.ByteString.singleton 236
    C237 ->
      Data.ByteString.singleton 237
    C238 ->
      Data.ByteString.singleton 238
    C239 ->
      Data.ByteString.singleton 239
    C240 ->
      Data.ByteString.singleton 240
    C241 ->
      Data.ByteString.singleton 241
    C242 ->
      Data.ByteString.singleton 242
    C243 ->
      Data.ByteString.singleton 243
    C244 ->
      Data.ByteString.singleton 244
    C245 ->
      Data.ByteString.singleton 245
    C246 ->
      Data.ByteString.singleton 246
    C247 ->
      Data.ByteString.singleton 247
    C248 ->
      Data.ByteString.singleton 248
    C249 ->
      Data.ByteString.singleton 249
    C250 ->
      Data.ByteString.singleton 250
    C251 ->
      Data.ByteString.singleton 251
    C252 ->
      Data.ByteString.singleton 252
    C253 ->
      Data.ByteString.singleton 253
    C254 ->
      Data.ByteString.singleton 254
    C255 ->
      Data.ByteString.singleton 255
