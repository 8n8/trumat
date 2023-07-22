module PlainBindMachine (Action(..), PlainBindMachine, start, step) where

import Transaction
import qualified TokenTag
import Data.Word (Word16, Word32)
import TokenTag (TokenTag)
import qualified ArgPatternMachine
import ParensPatternMachine (ParensPatternMachine)

start :: PlainBindMachine
start =
    Start


data PlainBindMachine
    = Start
    | BeforeArgument
    | InArgumentPattern PatternMachine
    deriving (Eq, Show)


data Action
    = Next PlainBindMachine [Transaction]
    | Fail
    | Done


step :: TokenTag -> PlainBindMachine -> Word16 -> Word16 -> Word32 -> Action
step token state row column tokenId =
    case (state, token) of
        (Start, TokenTag.Space) ->
            Next BeforeArgument []
        (Start, TokenTag.UpperName) ->
            Fail
        (BeforeArgument, TokenTag.Space) ->
            Next BeforeArgument []
        (Start, TokenTag.LowerName) ->
            Fail
        (BeforeArgument, TokenTag.UpperName) ->
            Next BeforeArgument [Transaction.ParameterChild tokenId]
        (Start, TokenTag.CloseParentheses) ->
            Fail
        (BeforeArgument, TokenTag.LowerName) ->
            Next BeforeArgument [Transaction.ParameterChild tokenId]
        (Start, TokenTag.OpenParentheses) ->
            Next (InArgumentPattern PatternMachine.start) []
        (InArgumentPattern patternState, _) ->
            case PatternMachine.step token patternState row column tokenId of
                PatternMachine.Next newPatternState transactions ->
                    Next (InArgumentPattern newPatternState) transactions

                PatternMachine.Done transactions ->
                    Next BeforeArgument transactions

                PatternMachine.Fail ->
                    Fail
