module BufferHelper exposing (Direction(..), Group(..), charsAround, indexFromPosition, isNonWordChar, isWhitespace, isWordChar, slice, stringCharAt, tuple3CharsPred, tuple3MapAll)

import Array exposing (Array)
import List.Extra
import Maybe.Extra
import Position exposing (Position)
import String.Extra
import Util.Array


stringCharAt : Int -> String -> Maybe Char
stringCharAt index string =
    String.slice index (index + 1) string
        |> String.uncons
        |> Maybe.map Tuple.first


charsAround : Int -> String -> ( Maybe Char, Maybe Char, Maybe Char )
charsAround index string =
    ( stringCharAt (index - 1) string
    , stringCharAt index string
    , stringCharAt (index + 1) string
    )


tuple3MapAll : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tuple3MapAll fn ( a1, a2, a3 ) =
    ( fn a1, fn a2, fn a3 )


tuple3CharsPred :
    (Char -> Bool)
    -> ( Maybe Char, Maybe Char, Maybe Char )
    -> ( Bool, Bool, Bool )
tuple3CharsPred pred =
    tuple3MapAll (Maybe.map pred >> Maybe.withDefault False)


{-| Internal function for getting the index of the position in a string

    indexFromPosition "reddish\ngreen" (Position 0 2)
    --> Just 2

    indexFromPosition "reddish\ngreen" (Position 1 2)
    --> Just 10

-}
indexFromPosition : String -> Position -> Maybe Int
indexFromPosition str position =
    -- Doesn't validate columns, only lines
    if position.line == 0 then
        Just position.column

    else
        String.indexes "\n" str
            |> List.Extra.getAt (position.line - 1)
            |> Maybe.map (\line -> line + position.column + 1)


slice : Position -> Position -> String -> Maybe String
slice pos1 pos2 str =
    let
        index1 =
            indexFromPosition str pos1

        index2 =
            indexFromPosition str pos2
    in
    case ( index1, index2 ) of
        ( Just i, Just j ) ->
            String.slice i j str |> Just

        ( _, _ ) ->
            Nothing



-- GROUPING


isWhitespace : Char -> Bool
isWhitespace =
    String.fromChar >> String.trim >> (==) ""


isNonWordChar : Char -> Bool
isNonWordChar =
    String.fromChar >> (\a -> String.contains a "/\\()\"':,.;<>~!@#$%^&*|+=[]{}`?-…")


isWordChar : Char -> Bool
isWordChar char =
    not (isNonWordChar char) && not (isWhitespace char)


type Group
    = None
    | Word
    | NonWord


type Direction
    = Forward
    | Backward
