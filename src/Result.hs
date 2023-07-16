module Result (Result (..)) where

data Result
  = Error String
  | Ok
  deriving (Eq, Show)
