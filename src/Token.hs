module Token (Token(..)) where

import IntLiteral (IntLiteral)
import Name (Name)

data Token
    = Module
    | Name Name
    | Exposing
    | OpenParentheses
    | CloseParentheses
    | Newline
    | Space
    | Equals
    | IntLiteral IntLiteral
