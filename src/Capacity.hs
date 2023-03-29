module Capacity (Capacity, k2, k20, k500, toInt) where


newtype Capacity
    = Capacity Int


k20 :: Capacity
k20 =
    Capacity 20000


k2 :: Capacity
k2 =
    Capacity 2000


k500 :: Capacity
k500 =
    Capacity 500000


toInt :: Capacity -> Int
toInt (Capacity capacity) =
    capacity
