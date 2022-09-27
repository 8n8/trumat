module SrcLength (SrcLength, init_, plus1, toInt) where


newtype SrcLength
    = SrcLength Int


plus1 :: SrcLength -> Maybe SrcLength
plus1 (SrcLength srcLength) =
    if srcLength == maxSrc then
        Nothing

    else
        Just (SrcLength (srcLength + 1))


toInt :: SrcLength -> Int
toInt (SrcLength srcLength) =
    srcLength


init_ :: SrcLength
init_ =
    SrcLength 0


{-| Say there are 10^6 lines of code, with 80 bytes in a line
on average.
-}
maxSrc :: Int
maxSrc =
    1000000 * 80
