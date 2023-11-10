module Buffer2 exposing
    ( Buffer(..)
    , init
    , insert
    , nearWordChar
    , replace
    , toString
    , valid
    )

import Array exposing (Array)
import Array.Util
import BufferHelper as BH
import List.Extra
import Maybe.Extra
import Position exposing (Position)
import String.Extra


{-| Returns True iff the buffer is valid

    valid (init "One\ntwo\nthree\nfour")
    --> True

    bb2 : Buffer
    bb2 = Buffer { content = "One\ntwo\nthree\nfour", lines = Array.fromList ["One","two!","three","four"] }

    valid bb2
    --> False

-}
type Buffer
    = Buffer { content : String, lines : Array String }


{-| Create a new buffer from a string

    import Array

    init "one\ntwo"
    --> Buffer { content = "one\ntwo", lines = Array.fromList ["one","two"] }

-}
init : String -> Buffer
init content =
    Buffer { content = content, lines = Array.fromList (String.lines content) }


valid : Buffer -> Bool
valid (Buffer data) =
    (data.lines |> Array.toList |> String.join "\n") == data.content


toString : Buffer -> String
toString (Buffer data) =
    data.content


{-| Returns true if the Position is at or after a word character. See isWordChar.

    import Position exposing(..)

    nearWordChar (Position 0 0) (init "one\ntwo")
    --> True

    nearWordChar (Position 0 10) (init "one\ntwo")
    --> False

-}
nearWordChar : Position -> Buffer -> Bool
nearWordChar position (Buffer data) =
    BH.indexFromPosition data.content position
        |> Maybe.andThen
            (\index ->
                let
                    previousChar =
                        BH.stringCharAt (index - 1) data.content

                    currentChar =
                        BH.stringCharAt index data.content
                in
                Maybe.map BH.isWordChar previousChar
                    |> Maybe.Extra.orElseLazy
                        (\() -> Maybe.map BH.isWordChar currentChar)
            )
        |> Maybe.withDefault False


{-| Insert a string into the buffer.

    import Array

    import Position

    bb: Buffer
    bb = init "aaa\nbbb"
    --> Buffer { content = "aaa\nbbb", lines = Array.fromList ["aaa","bbb"] }

    insert (Position 0 1) "X" bb
    --> Buffer { content = "aXaa\nbbb", lines = Array.fromList ["aXaa","bbb"] }

    valid bb2
    --> True

-}
insert : Position -> String -> Buffer -> Buffer
insert position str (Buffer data) =
    let
        content =
            BH.indexFromPosition data.content position
                |> Maybe.map (\index -> String.Extra.insertAt str index data.content)
                |> Maybe.withDefault data.content

        lines =
            Array.Util.insert position str data.lines
    in
    Buffer { content = content, lines = lines }


{-| Replace the string between two positions with a different string.

    import Array

    import Position


    bb : Buffer
    bb =
        init "One\ntwo\nthree\nfour"


    replace (Position 1 0) (Position 1 3) "TWO" bb
    -->   Buffer { content = "One\nTWO\nthree\nfour", lines = Array.fromList ["One","TWO","three","four"] }

    replace (Position 1 0) (Position 2 3) "TWO\nTHREE" bb

-}
replace : Position -> Position -> String -> Buffer -> Buffer
replace pos1 pos2 str (Buffer data) =
    let
        ( start, end ) =
            Position.order pos1 pos2

        content : String
        content =
            Maybe.map2
                (\startIndex endIndex ->
                    String.slice 0 startIndex data.content
                        ++ str
                        ++ String.dropLeft endIndex data.content
                )
                (BH.indexFromPosition data.content start)
                (BH.indexFromPosition data.content end)
                |> Maybe.withDefault data.content
    in
    Buffer { content = content, lines = Array.Util.replace start end str data.lines }



-- HELPER FUNCTIONS THAT DO NOT REFERENCE BUFFER --
