module Capacity (Capacity, fromInt, toInt) where


newtype Capacity
  = Capacity Int

toInt :: Capacity -> Int
toInt (Capacity int) =
  int

fromInt :: Int -> Maybe Capacity
fromInt int =
  if int < 1 then
    Nothing
  else
    Just (Capacity int)
