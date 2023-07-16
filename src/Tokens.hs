module Tokens (Tokens, Token, empty, fromChars) where

import Ambiguous (Ambiguous)
import qualified Ambiguous
import Array (Array)
import qualified Array
import Characters (Characters)
import qualified Characters
import Data.Word (Word16, Word32, Word8)
import ElmChar (ElmChar)
import qualified ElmChar
import Result

capacity :: Int
capacity =
  500 * 1000

empty :: IO Tokens
empty =
  do
    tags <- Array.empty capacity
    starts <- Array.empty capacity
    sizes <- Array.empty capacity
    return $ Tokens tags starts sizes

data Tokens = Tokens
  { tags :: Array Word8,
    starts :: Array Word32,
    sizes :: Array Word16
  }

fromChars :: Characters -> Tokens -> IO Result
fromChars characters tokens =
  tokenizeHelp characters tokens 0 Start

tokenizeHelp :: Characters -> Tokens -> Word32 -> Tokenizer -> IO Result
tokenizeHelp characters tokens characterIndex state =
  do
    charResult <- Characters.get (fromIntegral characterIndex) characters
    case charResult of
      Left _ ->
        pure Ok
      Right char ->
        case step characterIndex char state of
          Fail message ->
            pure (Error message)
          Next newState ->
            tokenizeHelp
              characters
              tokens
              (characterIndex + 1)
              newState
          Commit newTokens ->
            do
              saveTokensResult <- saveTokens newTokens tokens
              case saveTokensResult of
                Error error' ->
                  pure $ Error error'
                Ok ->
                  tokenizeHelp
                    characters
                    tokens
                    (characterIndex + 1)
                    Start

saveTokens :: [Token] -> Tokens -> IO Result
saveTokens newTokens tokens =
  case newTokens of
    [] ->
      pure Ok
    token : remainder ->
      do
        tagsResult <- Array.append (encodeTag token) (tags tokens)
        startsResult <- Array.append (encodeStart token) (starts tokens)
        sizesResult <- Array.append (encodeSize token) (sizes tokens)
        case (tagsResult, startsResult, sizesResult) of
          (Error error', _, _) ->
            pure (Error error')
          (_, Error error', _) ->
            pure (Error error')
          (_, _, Error error') ->
            pure (Error error')
          (Ok, Ok, Ok) ->
            saveTokens remainder tokens

encodeSize :: Token -> Word16
encodeSize token =
  case token of
    In ->
      0
    Module ->
      0
    Space ->
      0
    UpperName _ size ->
      size
    LowerName _ size ->
      size
    CloseParentheses ->
      0
    OpenParentheses ->
      0
    Newline ->
      0
    Equals ->
      0
    Number _ size ->
      size
    Else ->
      0
    If ->
      0
    Exposing ->
      0
    Of ->
      0
    Let ->
      0
    Port ->
      0

encodeStart :: Token -> Word32
encodeStart token =
  case token of
    Module ->
      0
    Space ->
      0
    UpperName start _ ->
      start
    LowerName start _ ->
      start
    CloseParentheses ->
      0
    OpenParentheses ->
      0
    Newline ->
      0
    Equals ->
      0
    Number size _ ->
      size
    Else ->
      0
    If ->
      0
    Exposing ->
      0
    Of ->
      0
    Let ->
      0
    Port ->
      0
    In ->
      0

encodeTag :: Token -> Word8
encodeTag token =
  case token of
    Module ->
      0
    Space ->
      1
    UpperName _ _ ->
      2
    LowerName _ _ ->
      3
    CloseParentheses ->
      4
    OpenParentheses ->
      5
    Newline ->
      6
    Equals ->
      7
    Number _ _ ->
      8
    Else ->
      9
    If ->
      10
    Exposing ->
      11
    Of ->
      12
    Let ->
      13
    Port ->
      14
    In ->
      15

data Action
  = Next Tokenizer
  | Commit [Token]
  | Fail String

data Token
  = UpperName Word32 Word16
  | LowerName Word32 Word16
  | CloseParentheses
  | OpenParentheses
  | Space
  | Module
  | Newline
  | Equals
  | Number Word32 Word16
  | Else
  | If
  | Exposing
  | Of
  | Let
  | Port
  | In

data Tokenizer
  = Start
  | Ambiguous Word32 Ambiguous
  | InUpperName Word32
  | InLowerName Word32
  | InHex Word32
  | HexStart Word32
  deriving (Show)

step :: Word32 -> ElmChar -> Tokenizer -> Action
step index char state =
  case (state, char) of
    (Start, ElmChar.M_) ->
      Next (Ambiguous index Ambiguous.M)
    (Ambiguous start Ambiguous.M, ElmChar.O_) ->
      Next (Ambiguous start Ambiguous.Mo)
    (Ambiguous start Ambiguous.Mo, ElmChar.D_) ->
      Next (Ambiguous start Ambiguous.Mod)
    (Ambiguous start Ambiguous.Mod, ElmChar.U_) ->
      Next (Ambiguous start Ambiguous.Modu)
    (Ambiguous start Ambiguous.Modu, ElmChar.L_) ->
      Next (Ambiguous start Ambiguous.Modul)
    (Ambiguous start Ambiguous.Modul, ElmChar.E_) ->
      Next (Ambiguous start Ambiguous.Module)
    (Ambiguous _ Ambiguous.Module, ElmChar.Space) ->
      Commit [Module, Space]
    (Start, ElmChar.X') ->
      Next (InUpperName index)
    (InUpperName start, ElmChar.Space) ->
      Commit
        [ UpperName start (fromIntegral (index - start)),
          Space
        ]
    (Start, ElmChar.E_) ->
      Next (Ambiguous index Ambiguous.E)
    (Ambiguous start Ambiguous.E, ElmChar.E_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.E_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.E_) ->
      Next (InUpperName start)
    (Start, ElmChar.O_) ->
      Next (Ambiguous index Ambiguous.O)
    (Ambiguous start Ambiguous.Ex, ElmChar.E_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.M_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.M_) ->
      Next (InUpperName start)
    (Start, ElmChar.D_) ->
      Next (InLowerName index)
    (Ambiguous start Ambiguous.Exp, ElmChar.D_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.O_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.O_) ->
      Next (InUpperName start)
    (Start, ElmChar.U_) ->
      Next (InLowerName index)
    (Ambiguous start Ambiguous.Expo, ElmChar.O_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.D_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.D_) ->
      Next (InUpperName start)
    (Start, ElmChar.L_) ->
      Next (Ambiguous index Ambiguous.L)
    (Ambiguous start Ambiguous.Expos, ElmChar.D_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.U_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.U_) ->
      Next (InUpperName start)
    (Start, ElmChar.Space) ->
      Commit [Space]
    (Ambiguous start Ambiguous.Exposi, ElmChar.U_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.L_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.L_) ->
      Next (InUpperName start)
    (Start, ElmChar.X_) ->
      Next (InLowerName index)
    (Ambiguous start Ambiguous.Exposin, ElmChar.X_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (InUpperName start, ElmChar.X') ->
      Next (InUpperName start)
    (Start, ElmChar.P_) ->
      Next (Ambiguous index Ambiguous.P)
    (Ambiguous start Ambiguous.Exposing, ElmChar.P_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.X') ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.X_) ->
      Next (InUpperName start)
    (Start, ElmChar.S_) ->
      Next (InLowerName index)
    (Ambiguous start Ambiguous.O, ElmChar.S_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.X_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.P_) ->
      Next (InUpperName start)
    (Start, ElmChar.I_) ->
      Next (Ambiguous index Ambiguous.I)
    (Ambiguous start Ambiguous.Of, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (InLowerName start, ElmChar.P_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.S_) ->
      Next (InUpperName start)
    (Start, ElmChar.N_) ->
      Next (InLowerName index)
    (Ambiguous start Ambiguous.El, ElmChar.S_) ->
      Next (Ambiguous start Ambiguous.Els)
    (InLowerName start, ElmChar.S_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.I_) ->
      Next (InUpperName start)
    (Start, ElmChar.G_) ->
      Next (InLowerName index)
    (Ambiguous start Ambiguous.Els, ElmChar.I_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.I_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.N_) ->
      Next (InUpperName start)
    (Start, ElmChar.OpenParentheses) ->
      Commit [OpenParentheses]
    (Ambiguous start Ambiguous.Else, ElmChar.N_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.N_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.G_) ->
      Next (InUpperName start)
    (Start, ElmChar.CloseParentheses) ->
      Commit [CloseParentheses]
    (Ambiguous start Ambiguous.L, ElmChar.G_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.G_) ->
      Next (InLowerName start)
    (InUpperName start, ElmChar.OpenParentheses) ->
      Commit
        [ UpperName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Start, ElmChar.Newline) ->
      Commit [Newline]
    (Ambiguous start Ambiguous.P, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (InLowerName start, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (InUpperName start, ElmChar.CloseParentheses) ->
      Commit
        [ UpperName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Start, ElmChar.Equals) ->
      Commit [Equals]
    (Ambiguous start Ambiguous.Po, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (InLowerName start, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (InUpperName start, ElmChar.Newline) ->
      Commit
        [ UpperName start (fromIntegral (index - start)),
          Newline
        ]
    (Start, ElmChar.Zero) ->
      Next (Ambiguous index Ambiguous.Zero)
    (Ambiguous start Ambiguous.Zero, ElmChar.Newline) ->
      Commit [Number start 1, Newline]
    (Ambiguous start Ambiguous.Por, ElmChar.Newline) ->
      Commit [LowerName start 3, Newline]
    (InLowerName start, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (InUpperName start, ElmChar.Equals) ->
      Commit
        [ UpperName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous _ Ambiguous.Zero, ElmChar.M_) ->
      Fail "invalid token: 0m"
    (Ambiguous start Ambiguous.Port, ElmChar.M_) ->
      Next (InLowerName start)
    (InLowerName start, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (InUpperName start, ElmChar.Zero) ->
      Next (InUpperName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.O_) ->
      Fail "invalid token: 0o"
    (Ambiguous _ Ambiguous.I, ElmChar.Zero) ->
      Fail "invalid token: i0"
    (InLowerName start, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.D_) ->
      Fail "invalid token: 0d"
    (Ambiguous start Ambiguous.If, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.U_) ->
      Fail "invalid token: 0u"
    (Ambiguous start Ambiguous.M, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.L_) ->
      Fail "invalid token: 0l"
    (Ambiguous start Ambiguous.Mo, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.E_) ->
      Fail "invalid token: 0e"
    (Ambiguous start Ambiguous.Mod, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Zero, ElmChar.Space) ->
      Commit [Number start 1, Space]
    (Ambiguous start Ambiguous.Modu, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Zero, ElmChar.X_) ->
      Next (HexStart start)
    (InHex _, ElmChar.X_) ->
      Fail "invalid token: 0x..x"
    (HexStart _, ElmChar.X_) ->
      Fail "invalid token: 0x..x"
    (Ambiguous start Ambiguous.Modu, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.M_) ->
      Fail "invalid token: 0x..m"
    (HexStart _, ElmChar.M_) ->
      Fail "invalid token: 0x..m"
    (Ambiguous start Ambiguous.Modul, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.O_) ->
      Fail "invalid token: 0x..o"
    (HexStart _, ElmChar.O_) ->
      Fail "invalid token: 0x..o"
    (Ambiguous start Ambiguous.Module, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex start, ElmChar.D_) ->
      Next (InHex start)
    (HexStart start, ElmChar.D_) ->
      Next (InHex start)
    (Ambiguous start Ambiguous.E, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.U_) ->
      Fail "invalid token: 0x..u"
    (HexStart _, ElmChar.U_) ->
      Fail "invalid token: 0x..u"
    (Ambiguous start Ambiguous.Ex, ElmChar.L_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.L_) ->
      Fail "invalid token: 0x..l"
    (HexStart _, ElmChar.L_) ->
      Fail "invalid token: 0x..l"
    (Ambiguous start Ambiguous.Exp, ElmChar.O_) ->
      Next (Ambiguous start Ambiguous.Expo)
    (InHex start, ElmChar.E_) ->
      Next (InHex start)
    (HexStart start, ElmChar.E_) ->
      Next (InHex start)
    (Ambiguous start Ambiguous.Expo, ElmChar.D_) ->
      Next (InLowerName start)
    (InHex start, ElmChar.Space) ->
      Commit
        [ Number start (fromIntegral (index - start)),
          Space
        ]
    (HexStart _, ElmChar.Space) ->
      Fail "invalid token: 0x"
    (Ambiguous start Ambiguous.Expos, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.X') ->
      Fail "invalid token: 0x..X"
    (HexStart _, ElmChar.X') ->
      Fail "invalid token: 0x..X"
    (Ambiguous start Ambiguous.Exposi, ElmChar.O_) ->
      Next (InLowerName start)
    (HexStart _, ElmChar.P_) ->
      Fail "invalid token: 0x..p"
    (InHex _, ElmChar.P_) ->
      Fail "invalid token: 0x..p"
    (Ambiguous start Ambiguous.Exposin, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.S_) ->
      Fail "invalid token: 0x..s"
    (HexStart _, ElmChar.S_) ->
      Fail "invalid token: 0x..s"
    (Ambiguous start Ambiguous.Exposing, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.I_) ->
      Fail "invalid token: 0x..i"
    (HexStart _, ElmChar.I_) ->
      Fail "invalid token: 0x..i"
    (Ambiguous start Ambiguous.O, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.N_) ->
      Fail "invalid token: 0x..n"
    (HexStart _, ElmChar.N_) ->
      Fail "invalid token: 0x..n"
    (Ambiguous start Ambiguous.Of, ElmChar.O_) ->
      Next (InLowerName start)
    (InHex _, ElmChar.G_) ->
      Fail "invalid token: 0x..g"
    (HexStart _, ElmChar.G_) ->
      Fail "invalid token: 0x..g"
    (Ambiguous start Ambiguous.El, ElmChar.O_) ->
      Next (InLowerName start)
    (HexStart _, ElmChar.OpenParentheses) ->
      Fail "invalid token 0x"
    (InHex start, ElmChar.OpenParentheses) ->
      Commit
        [ Number start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Els, ElmChar.O_) ->
      Next (InLowerName start)
    (HexStart _, ElmChar.CloseParentheses) ->
      Fail "invalid token 0x"
    (InHex start, ElmChar.CloseParentheses) ->
      Commit
        [ Number start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Else, ElmChar.O_) ->
      Next (InLowerName start)
    (HexStart _, ElmChar.Newline) ->
      Fail "invalid token: 0x"
    (InHex start, ElmChar.Newline) ->
      Commit
        [ Number start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.L, ElmChar.O_) ->
      Next (InLowerName start)
    (HexStart _, ElmChar.Equals) ->
      Fail "invalid token: 0x"
    (InHex start, ElmChar.Equals) ->
      Commit
        [ Number start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.P, ElmChar.O_) ->
      Next (Ambiguous start Ambiguous.Po)
    (HexStart start, ElmChar.Zero) ->
      Next (InHex start)
    (InHex start, ElmChar.Zero) ->
      Next (InHex start)
    (Ambiguous start Ambiguous.Po, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.X') ->
      Fail "invalid token: 0X"
    (Ambiguous start Ambiguous.Modu, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.P_) ->
      Fail "invalid token: 0p"
    (Ambiguous start Ambiguous.Modu, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Of, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.S_) ->
      Fail "invalid token: 0s"
    (Ambiguous start Ambiguous.Modu, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.Space) ->
      Commit [LowerName start (fromIntegral (index - start)), Space]
    (Ambiguous start Ambiguous.Mo, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Mod, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous _ Ambiguous.Zero, ElmChar.I_) ->
      Fail "invalid token: 0i"
    (Ambiguous start Ambiguous.Modu, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Module, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.L_) ->
      Next (Ambiguous start Ambiguous.El)
    (Ambiguous start Ambiguous.E, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.E, ElmChar.X_) ->
      Next (Ambiguous start Ambiguous.Ex)
    (Ambiguous start Ambiguous.E, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Ex, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.P_) ->
      Next (Ambiguous start Ambiguous.Exp)
    (Ambiguous start Ambiguous.Exp, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Exp, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Expo, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Expos, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Exposi, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Exposin, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Exposing, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.El, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.E_) ->
      Next (Ambiguous start Ambiguous.Else)
    (Ambiguous start Ambiguous.Els, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Els, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Else, ElmChar.Space) ->
      Commit [Else, Space]
    (Ambiguous start Ambiguous.Else, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.E_) ->
      Next (Ambiguous start Ambiguous.Le)
    (Ambiguous start Ambiguous.L, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.L, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Le, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Por, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Por, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Port, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.I, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.If, ElmChar.Space) ->
      Commit [If, Space]
    (Ambiguous start Ambiguous.If, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Zero, ElmChar.N_) ->
      Fail "invalid token: 0n"
    (Ambiguous _ Ambiguous.Zero, ElmChar.G_) ->
      Fail "invalid token: 0g"
    (Ambiguous start Ambiguous.Zero, ElmChar.OpenParentheses) ->
      Commit
        [ Number start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Zero, ElmChar.CloseParentheses) ->
      Commit
        [ Number start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Modu, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Modu, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modu, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modu, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.S_) ->
      Next (Ambiguous start Ambiguous.Expos)
    (Ambiguous start Ambiguous.Expo, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.I_) ->
      Next (Ambiguous start Ambiguous.Exposi)
    (Ambiguous start Ambiguous.Expos, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.N_) ->
      Next (Ambiguous start Ambiguous.Exposin)
    (Ambiguous start Ambiguous.Exposi, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.G_) ->
      Next (Ambiguous start Ambiguous.Exposing)
    (Ambiguous start Ambiguous.Exposing, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Por, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.N_) ->
      Next (Ambiguous start Ambiguous.In)
    (Ambiguous start Ambiguous.If, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.If, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mo, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Mod, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Zero, ElmChar.Equals) ->
      Commit [Number start (fromIntegral (index - start)), Equals]
    (Ambiguous _ Ambiguous.Zero, ElmChar.Zero) ->
      Fail "invalid token: 00"
    (Ambiguous start Ambiguous.Modu, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modu, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modul, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Module, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Module, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Module, ElmChar.OpenParentheses) ->
      Commit [Module, OpenParentheses]
    (Ambiguous start Ambiguous.E, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.E, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.E, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.E, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Ex, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Ex, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Ex, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Ex, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Exp, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Exp, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Exp, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Exp, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Expo, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Expo, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Expo, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Expo, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Expos, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Expos, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Expos, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Expos, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Exposi, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Exposi, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Exposi, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Exposi, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Exposin, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Exposin, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Exposin, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Exposin, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous _ Ambiguous.Exposing, ElmChar.OpenParentheses) ->
      Commit [Exposing, OpenParentheses]
    (Ambiguous _ Ambiguous.Exposing, ElmChar.CloseParentheses) ->
      Commit [Exposing, CloseParentheses]
    (Ambiguous _ Ambiguous.Exposing, ElmChar.Newline) ->
      Commit [Exposing, Newline]
    (Ambiguous _ Ambiguous.Exposing, ElmChar.Equals) ->
      Commit [Exposing, Equals]
    (Ambiguous start Ambiguous.O, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.O, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.O, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.O, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous _ Ambiguous.Of, ElmChar.OpenParentheses) ->
      Commit [Of, OpenParentheses]
    (Ambiguous _ Ambiguous.Of, ElmChar.CloseParentheses) ->
      Commit [Of, CloseParentheses]
    (Ambiguous _ Ambiguous.Of, ElmChar.Newline) ->
      Commit [Of, Newline]
    (Ambiguous _ Ambiguous.Of, ElmChar.Equals) ->
      Commit [Of, Equals]
    (Ambiguous start Ambiguous.El, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.El, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.El, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.El, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Els, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Els, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Els, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Els, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous _ Ambiguous.Else, ElmChar.OpenParentheses) ->
      Commit [Else, OpenParentheses]
    (Ambiguous _ Ambiguous.Else, ElmChar.CloseParentheses) ->
      Commit [Else, CloseParentheses]
    (Ambiguous _ Ambiguous.Else, ElmChar.Newline) ->
      Commit [Else, Newline]
    (Ambiguous _ Ambiguous.Else, ElmChar.Equals) ->
      Commit [Else, Equals]
    (Ambiguous start Ambiguous.L, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.L, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.L, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.L, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.P, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Le, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.Space) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Space
        ]
    (Ambiguous start Ambiguous.Le, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Let, ElmChar.Space) ->
      Commit [Let, Space]
    (Ambiguous start Ambiguous.Let, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Por, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Por, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Por, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Por, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Port, ElmChar.OpenParentheses) ->
      Commit [Port, OpenParentheses]
    (Ambiguous _ Ambiguous.Port, ElmChar.CloseParentheses) ->
      Commit [Port, CloseParentheses]
    (Ambiguous _ Ambiguous.Port, ElmChar.Newline) ->
      Commit [Port, Newline]
    (Ambiguous _ Ambiguous.Port, ElmChar.Equals) ->
      Commit [Port, Equals]
    (Ambiguous start Ambiguous.I, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.I, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.I, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous _ Ambiguous.If, ElmChar.OpenParentheses) ->
      Commit [If, OpenParentheses]
    (Ambiguous _ Ambiguous.If, ElmChar.CloseParentheses) ->
      Commit [If, CloseParentheses]
    (Ambiguous _ Ambiguous.If, ElmChar.Newline) ->
      Commit [If, Newline]
    (Ambiguous _ Ambiguous.If, ElmChar.Equals) ->
      Commit [If, Equals]
    (Ambiguous start Ambiguous.In, ElmChar.M_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.O_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.D_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.U_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.M, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.M, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.M, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Mo, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Mo, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Mo, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Mo, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Mod, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Mod, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Mod, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Mod, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modu, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modu, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Modu, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Modu, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Modul, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Modul, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Modul, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Modul, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Module, ElmChar.CloseParentheses) ->
      Commit [Module, CloseParentheses]
    (Ambiguous _ Ambiguous.Module, ElmChar.Newline) ->
      Commit [Module, Newline]
    (Ambiguous _ Ambiguous.Module, ElmChar.Equals) ->
      Commit [Module, Equals]
    (Ambiguous start Ambiguous.Module, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.E, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Ex, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exp, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expo, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Expos, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposi, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposin, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Exposing, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.O, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Of, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.El, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Els, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Else, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.L, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.P, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Le, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Po, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Po, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Po, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Port, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.I, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.If, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.L_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.E_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.M, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Modu, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Modu, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.P, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Le, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Le, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous start Ambiguous.Let, ElmChar.G_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Let, ElmChar.OpenParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          OpenParentheses
        ]
    (Ambiguous start Ambiguous.Let, ElmChar.CloseParentheses) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          CloseParentheses
        ]
    (Ambiguous start Ambiguous.Let, ElmChar.Newline) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Newline
        ]
    (Ambiguous _ Ambiguous.In, ElmChar.Space) ->
      Commit [In, Space]
    (Ambiguous start Ambiguous.In, ElmChar.X') ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.X_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.P_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.Le, ElmChar.Equals) ->
      Commit
        [ LowerName start (fromIntegral (index - start)),
          Equals
        ]
    (Ambiguous start Ambiguous.Le, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.Let, ElmChar.Equals) ->
      Commit [Let, Equals]
    (Ambiguous start Ambiguous.Let, ElmChar.Zero) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.S_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.I_) ->
      Next (InLowerName start)
    (Ambiguous start Ambiguous.In, ElmChar.N_) ->
      Next (InLowerName start)
    (Ambiguous _ Ambiguous.In, ElmChar.OpenParentheses) ->
      Commit [In, OpenParentheses]
    (Ambiguous _ Ambiguous.In, ElmChar.CloseParentheses) ->
      Commit [In, CloseParentheses]
    (Ambiguous _ Ambiguous.In, ElmChar.Newline) ->
      Commit [In, Newline]
    (Ambiguous _ Ambiguous.In, ElmChar.Equals) ->
      Commit [In, Equals]
    (Ambiguous start Ambiguous.In, ElmChar.Zero) ->
      Next (InLowerName start)
