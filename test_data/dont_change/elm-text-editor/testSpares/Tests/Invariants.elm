module Tests.Invariants exposing
    ( cursorColumnIsAlwaysPositive
    , cursorLineIsAlwaysPositive
    , cursorLineNeverGetsToBeyondEndOfLine
    , cursorLineNeverGetsToNonexistingLine
    )

import ArchitectureTest exposing (..)
import Buffer
import Editor exposing (..)
import Expect exposing (Expectation)
import List.Extra
import Test exposing (..)
import Tests.Common exposing (..)


cursorLineIsAlwaysPositive : Test
cursorLineIsAlwaysPositive =
    invariantTest "cursor.line is always positive" app <|
        \_ _ finalModel ->
            let
                cursor =
                    Editor.getCursor finalModel
            in
            cursor.line
                |> Expect.atLeast 0


cursorColumnIsAlwaysPositive : Test
cursorColumnIsAlwaysPositive =
    invariantTest "cursor.column is always positive" app <|
        \_ _ finalModel ->
            let
                cursor =
                    Editor.getCursor finalModel
            in
            cursor.column
                |> Expect.atLeast 0


cursorLineNeverGetsToNonexistingLine : Test
cursorLineNeverGetsToNonexistingLine =
    invariantTest "cursor.line never gets to nonexisting line" app <|
        \_ _ finalModel ->
            let
                cursor =
                    Editor.getCursor finalModel

                lastLine =
                    Editor.getBuffer finalModel
                        |> Buffer.lastPosition
                        |> .line
            in
            cursor.line
                |> Expect.atMost lastLine


cursorLineNeverGetsToBeyondEndOfLine : Test
cursorLineNeverGetsToBeyondEndOfLine =
    invariantTest "cursor.line never gets beyond end of line" app <|
        \_ _ finalModel ->
            let
                cursor =
                    Editor.getCursor finalModel

                currentLineLength : Int
                currentLineLength =
                    Editor.getBuffer finalModel
                        |> Buffer.lines
                        |> List.Extra.getAt cursor.line
                        |> Maybe.map String.length
                        |> Maybe.withDefault 1000000000
            in
            cursor.column
                |> Expect.atMost currentLineLength
