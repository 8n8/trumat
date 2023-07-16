module TokenTag (TokenTag (..)) where

data TokenTag
  = UpperName
  | LowerName
  | CloseParentheses
  | OpenParentheses
  | Space
  | Module
  | Newline
  | Equals
  | Number
  | Else
  | If
  | Exposing
  | Of
  | Let
  | Port
  | In
  deriving (Eq, Show)
