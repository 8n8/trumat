module NonEmpty (NonEmpty, fromList) where


data NonEmpty a =
    NonEmpty a [a]


fromList :: [a] -> Maybe (NonEmpty a)
fromList list =
    case list of
        [] ->
            Nothing

        head_ : tail_ ->
            Just $ NonEmpty head_ tail_
