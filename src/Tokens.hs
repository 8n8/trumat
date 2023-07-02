module Tokens (parse, Token(..)) where

import Prelude
import Data.Text (Text)
import ElmChars (ElmChar)

data State
    = Init

data Token
    = Module
    | Space1
    | Name Text
    | Exposing
    | OpenParentheses
    | CloseParentheses
    | Newline3
    | Equals
    | Newline1
    | Space4

parseStep :: ElmChar -> State -> Either String (State, Maybe Token)
parseStep char state =
    case (char, state) of
        (ElmChar.Ml, Init) ->
            Right (ModuleStartsWithM, 

parseHelp :: [ElmChar] -> State -> [Token] -> Either String [Token]
parseHelp raw state accumulated =
    case raw of
        [] ->
            Right $ reverse accumulated

        first : remainder ->
            case parseStep first state of
                Left err ->
                    Left err

                Right (newState, token) ->
                    parseHelp remainder newState (token : accumulated)

parse :: [ElmChar] -> Either String [Token]
parse raw =
    parseHelp raw Init []
