module EditorTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Tests.CursorLeft as CursorLeft
import Tests.CursorRight as CursorRight
import Tests.Insert as Insert
import Tests.Invariants as Invariants
import Tests.NoOp as NoOp



--suite : Test
--suite =
--    describe "String.reverse"
--        -- Nest as many descriptions as you like.
--        [ test "has no effect on a palindrome" <|
--            \_ ->
--                let
--                    palindrome =
--                        "hannah"
--                in
--                Expect.equal palindrome (String.reverse palindrome)
--        ]
--


suite : Test
suite =
    concat
        [ --        describe "Invariants"
          --            [ describe "cursor"
          --                [ describe "line"
          --                    []
          --                , describe "column"
          --                    []
          --                ]
          --            ]
          describe "skip"
            [ describe "NoOp"
                [ NoOp.doesNothing
                ]
            , describe "Invariants"
                [ Invariants.cursorLineIsAlwaysPositive
                , Invariants.cursorColumnIsAlwaysPositive
                , Invariants.cursorLineNeverGetsToNonexistingLine
                , Invariants.cursorLineNeverGetsToBeyondEndOfLine
                ]
            , describe "Insert"
                [ Insert.one ]
            , describe
                "CursorLeft"
                [ CursorLeft.notNegativeColumn ]
            , describe "CursorRight"
                [ CursorRight.advance ]
            ]
        ]
