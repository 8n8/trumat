module Tests.Insert exposing (one)

import ArchitectureTest exposing (..)
import Editor
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tests.Common exposing (..)


one : Test
one =
    msgTest "inserting one character properly advances the cursor" app insertOne <|
        \initialModel _ finalModel ->
            let
                initialCursor =
                    Editor.getCursor initialModel

                finalCursor =
                    Editor.getCursor finalModel

                ok =
                    case ( finalCursor.line - initialCursor.line, finalCursor.column - initialCursor.column, finalCursor.column ) of
                        ( 0, 1, _ ) ->
                            True

                        ( 0, _, _ ) ->
                            False

                        ( 1, _, 0 ) ->
                            True

                        _ ->
                            False
            in
            ok
                |> Expect.equal True
