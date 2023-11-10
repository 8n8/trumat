module UnitTest exposing (suite)

import Buffer exposing (..)
import Debounce
import Editor exposing (EditorConfig, EditorMsg)
import Editor.Config exposing (WrapOption(..))
import Editor.Function as F exposing (bufferOf, stateOf)
import Editor.History
import Editor.Model exposing (InternalState)
import Editor.Update exposing (Msg(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Position exposing (Position)
import RollingList
import Test exposing (..)


testBuffer =
    Buffer "a\nbc"


type Msg
    = EditorMsg EditorMsg


editorConfig : EditorConfig Msg
editorConfig =
    { editorMsg = EditorMsg
    , width = 450
    , height = 544
    , lineHeight = 16.0
    , showInfoPanel = False
    , wrapParams = { maximumWidth = 55, optimalWidth = 50, stringWidth = String.length }
    , wrapOption = DontWrap
    }


state : InternalState
state =
    { config = Editor.transformConfig editorConfig
    , cursor = Position 0 0
    , selection = Nothing
    , selectedText = Nothing
    , clipboard = ""
    , currentLine = Nothing
    , dragging = False
    , history = Editor.History.empty
    , searchTerm = ""
    , replacementText = ""
    , canReplace = False
    , searchResults = RollingList.fromList []
    , showHelp = True
    , showInfoPanel = editorConfig.showInfoPanel
    , showGoToLinePanel = False
    , showSearchPanel = False
    , savedBuffer = Buffer.fromString "abc\ndef"
    , debounce = Debounce.init
    , topLine = 0
    , searchHitIndex = 0
    }


suite : Test
suite =
    describe "Editor update"
        [ describe "CursorForward"
            -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            , test "Cursor forward, inside line" <|
                \_ ->
                    let
                        buffer =
                            Buffer.fromString "abc\ndef"

                        state1 =
                            { state | cursor = Position 0 1 }

                        state2 =
                            F.cursorRight state1 buffer |> stateOf
                    in
                    Expect.equal state2.cursor (Position 0 2)
            , test "Cursor forward, inside line, correct character" <|
                \_ ->
                    let
                        buffer =
                            Buffer.fromString "abc\ndef"

                        state1 =
                            { state | cursor = Position 0 1 }

                        state2 =
                            F.cursorRight state1 buffer |> stateOf
                    in
                    Expect.equal (String.slice 2 3 (Buffer.toString buffer)) "c"
            , test "Cursor forward just before end of line" <|
                \_ ->
                    let
                        buffer =
                            Buffer.fromString "abc\ndef"

                        state1 =
                            { state | cursor = Position 0 2 }

                        state2 =
                            F.cursorRight state1 buffer |> stateOf
                    in
                    Expect.equal state2.cursor (Position 0 3)
            ]
        ]
