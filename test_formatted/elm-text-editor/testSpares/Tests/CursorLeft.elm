module Tests.CursorLeft exposing (notNegativeColumn)

import ArchitectureTest exposing (..)
import Buffer
import Editor exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tests.Common exposing (..)


retreat : Test
retreat =
    msgTest "CursorLeft moves the cursor back one unit" app cursorLeft <|
        \initialModel cursorLeft finalModel ->
            let
                initialCursor =
                    Editor.getCursor initialModel

                finalCursor =
                    Editor.getCursor finalModel

                finallLineLength =
                    Buffer.lineEnd initialCursor.line (Editor.getBuffer initialModel)

                cursorWasAtEndOfLine =
                    -- ask if the initial cursor was at the end of the line
                    case Maybe.map2 (-) finallLineLength (Just finalCursor.column) of
                        Just 0 ->
                            True

                        _ ->
                            False

                ok =
                    case ( finalCursor.line - initialCursor.line, initialCursor.column - finalCursor.column, cursorWasAtEndOfLine ) of
                        ( 0, 1, _ ) ->
                            -- move back in same line
                            True

                        ( 1, _, True ) ->
                            -- move back to end of previous line
                            True

                        ( 0, 0, _ ) ->
                            -- cursor was at beginning of text: can't move any more
                            True

                        _ ->
                            False
            in
            ok
                |> Expect.equal True


notNegativeColumn : Test
notNegativeColumn =
    msgTest "CursorLeft never results in negative cursor columns" app cursorLeft <|
        \_ _ finalModel ->
            let
                finalCursor =
                    Editor.getCursor finalModel
            in
            finalCursor.column
                |> Expect.greaterThan -1
