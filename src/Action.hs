module Action (Action (..)) where

data Action
  = MoveRight
  | Delete
  | Finish
  | InsertSpace
  | Fail String
  | InsertNewline
  deriving (Show)
