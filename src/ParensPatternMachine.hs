module ParensPatternMachine (Action(..), ParensPatternMachine, start, step) where

import Data.Word (Word32, Word16)
import Transaction
import qualified TokenTag
import TokenTag (TokenTag)

start :: ParensPatternMachine
start =
    Start


data Action
    = Next ParensPatternMachine [Transaction]
    | Fail
    | Done [Transaction]


data ParensPatternMachine
    = Start
    | AfterOpeningParens
    deriving (Show, Eq)


step :: TokenTag -> ParensPatternMachine -> Word16 -> Word16 -> Word32 -> Action
step token state row column tokenId =
    case (state, token) of
        (Start, TokenTag.CloseParentheses) ->
            Fail
        (Start, TokenTag.UpperName) ->
            Done [Transaction.Token tokenId]
        (Start, TokenTag.LowerName) ->
            Done [Transaction.Token tokenId]
        (Start, TokenTag.OpenParentheses) ->
            Next AfterOpeningParens []
        (AfterOpeningParens, TokenTag.Space) ->
            Next AfterOpeningParens []
        (Start, TokenTag.Space) ->
            Next Start []
        (AfterOpeningParens, TokenTag.UpperName) ->
