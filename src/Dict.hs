module Dict (Dict, empty, get) where

import Array (Array)
import qualified Array
import Foreign.Storable (Storable)
import Data.Word (Word32)

data Dict
    = Dict (Array Word32) (Array Word32)

empty :: Int -> IO Dict
empty capacity =
    do
    left <- Array.empty capacity
    right <- Array.empty capacity
    pure (Dict left right)

get :: Word32 -> Dict -> IO (Maybe Word32)
get key (Dict left right) =
    do
    indexResult <- getMatchingIndex key left 0
    case indexResult of
        Nothing ->
            pure Nothing

        Just index ->
            do
            valueResult <- Array.get index right
            case valueResult of
                Left _ ->
                    pure Nothing

                Right value ->
                    pure $ Just value


getMatchingIndex :: (Storable a, Eq a) => a -> Array a -> Int -> IO (Maybe Int)
getMatchingIndex key array index =
    do
    result <- Array.get index array
    case result of
        Left _ ->
            pure Nothing

        Right item ->
            if item == key then
                pure $ Just index

            else
                getMatchingIndex key array (index + 1)

