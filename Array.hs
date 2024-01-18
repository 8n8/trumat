module Array (Array, Index, get, init)


newtype Array member
  = Array (Ptr member)


newtype Index
  = Index Int


get :: Index -> Array member -> member


init :: accumulated -> (accumulated -> Index -> (member, accumulated)) -> Array member
init initialState makeItem =
