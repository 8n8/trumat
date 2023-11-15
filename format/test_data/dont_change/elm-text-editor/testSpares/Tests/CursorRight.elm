module Tests.CursorRight exposing (advance)

import ArchitectureTest exposing (..)
import Buffer
import Editor
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tests.Common exposing (..)


advance : Test
advance =
    msgTest "CursorRight advances the cursor" app cursorRight <|
        \initialModel _ finalModel ->
            let
                initialCursor =
                    Editor.getCursor initialModel

                finalCursor =
                    Editor.getCursor finalModel

                ok =
                    case ( finalCursor.line - initialCursor.line, finalCursor.column - initialCursor.column, finalCursor.column ) of
                        ( 0, 1, _ ) ->
                            -- advances cursor by 1 but does not change line
                            True

                        ( 1, _, 0 ) ->
                            -- moves to beginning of next line (WEAK)
                            True

                        ( 0, _, _ ) ->
                            False

                        _ ->
                            False
            in
            ok
                |> Expect.equal True



--
--doesNotAdvanceBeyondEndOfLine : Test
--doesNotAdvanceBeyondEndOfLine =
--    msgTest "CursorRight doesn't advance the cursor beyond the end of the line " app cursorRight <|
--        \initialModel _ finalModel ->
--            let
--                initialCursor =
--                    Editor.getCursor initialModel
--
--                finalCursor =
--                    Editor.getCursor finalModel
--
--                initialLineLength =
--                    Buffer.lineEnd initialCursor.line (Editor.getBuffer initialModel)
--
--                cursorWasAtEndOfLine =
--                    -- ask if the initial cursor was at the end of the line
--                    case Maybe.map2 (-) initialLineLength (Just initialCursor.column) of
--                        Just 0 ->
--                            True
--
--                        _ ->
--                            False
--
--                ok =
--                    case ( finalCursor.line - initialCursor.line, finalCursor.column - initialCursor.column, cursorWasAtEndOfLine ) of
--                        ( 0, 1, _ ) ->
--                            -- advances cursor by 1 but does not change line
--                            True
--
--                        ( 1, _, True ) ->
--                            -- moves to beginning of next line (WEAK)
--                            True
--
--                        ( 0, _, _ ) ->
--                            False
--
--                        _ ->
--                            False
--            in
--            ok
--                |> Expect.equal True
