module Tests.Common exposing (Msg(..), app, config, cursorDown, cursorLeft, cursorRight, cursorUp, firstLine, initModel, insert, insertBlank, insertOne, lastLine, modelTemplate, modelToString, msgTostring, noOp, oneCharString, removeCharAfter, removeCharBefore, renderVisible, weightedMsgFuzzer)

import ArchitectureTest exposing (..)
import Editor exposing (..)
import Editor.Config exposing (WrapOption(..))
import Editor.Update exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import String.Interpolate exposing (interpolate)


app : TestedApp Editor EditorMsg
app =
    { model = ConstantModel initModel
    , update = NormalUpdate Editor.update
    , msgFuzzer = Fuzz.constant CursorRight -- weightedMsgFuzzer
    , msgToString = msgTostring
    , modelToString = modelToString
    }


type Msg
    = EditorMsg EditorMsg


modelToString : Editor -> String
modelToString editor =
    let
        pos =
            getCursor editor

        line =
            pos.line |> String.fromInt

        column =
            pos.column |> String.fromInt

        source =
            getSource editor

        decoratedSource =
            source
                |> String.replace " " "°"
                |> String.lines
                |> List.map (\line_ -> (line_ |> String.length |> String.fromInt) ++ ": " ++ line_)
                |> List.indexedMap (\i line_ -> String.fromInt i ++ ", " ++ line_)
                |> String.join "\n"
    in
    interpolate modelTemplate [ line, column, decoratedSource ]


renderVisible : String -> String
renderVisible str =
    str
        |> String.replace " " "°"
        |> String.replace "\n" "(NL)"


modelTemplate =
    "MODEL\ncursor = ({0}, {1})\nbuffer:\n{2}\n"


msgTostring : EditorMsg -> String
msgTostring editorMsg =
    case editorMsg of
        NoOp ->
            "NoOp"

        CursorUp ->
            "CursorUp"

        CursorDown ->
            "CursorDown"

        CursorLeft ->
            "CursorLeft"

        CursorRight ->
            "CursorRight"

        Insert str ->
            "Insert:  " ++ renderVisible str

        RemoveCharBefore ->
            "RemoveCharBefore"

        RemoveCharAfter ->
            "RemoveCharAfter"

        FirstLine ->
            "CursorRight"

        LastLine ->
            "CursorRight"

        _ ->
            "Undefined"


initModel : Editor
initModel =
    Editor.init config "a"



-- "abc\ndefg\n"


config : EditorConfig Msg
config =
    { editorMsg = EditorMsg
    , width = 450
    , height = 544
    , lineHeight = 16.0
    , showInfoPanel = False
    , wrapParams = { maximumWidth = 55, optimalWidth = 50, stringWidth = String.length }
    , wrapOption = DontWrap
    }


weightedMsgFuzzer : Fuzzer EditorMsg
weightedMsgFuzzer =
    Fuzz.frequency
        [ ( 1, noOp )
        , ( 1, firstLine )
        , ( 1, lastLine )
        , ( 1, cursorUp )
        , ( 1, cursorDown )
        , ( 1, cursorLeft )
        , ( 1, cursorRight )
        , ( 10, insertOne )
        , ( 2, insertBlank )
        , ( 1, removeCharBefore )
        , ( 1, removeCharAfter )
        ]


noOp : Fuzzer EditorMsg
noOp =
    Fuzz.constant NoOp


cursorLeft : Fuzzer EditorMsg
cursorLeft =
    Fuzz.constant CursorLeft


cursorRight : Fuzzer EditorMsg
cursorRight =
    Fuzz.constant CursorRight


cursorUp : Fuzzer EditorMsg
cursorUp =
    Fuzz.constant CursorUp


cursorDown : Fuzzer EditorMsg
cursorDown =
    Fuzz.constant CursorDown


firstLine : Fuzzer EditorMsg
firstLine =
    Fuzz.constant FirstLine


lastLine : Fuzzer EditorMsg
lastLine =
    Fuzz.constant LastLine


insertOne : Fuzzer EditorMsg
insertOne =
    Fuzz.string |> Fuzz.map oneCharString |> Fuzz.map Insert


oneCharString : String -> String
oneCharString str =
    case String.left 1 str of
        "" ->
            " "

        _ ->
            str


insert : Fuzzer EditorMsg
insert =
    Fuzz.string |> Fuzz.map Insert


insertBlank : Fuzzer EditorMsg
insertBlank =
    Fuzz.constant " " |> Fuzz.map Insert


removeCharBefore : Fuzzer EditorMsg
removeCharBefore =
    Fuzz.constant RemoveCharBefore


removeCharAfter : Fuzzer EditorMsg
removeCharAfter =
    Fuzz.constant RemoveCharAfter



--
--hover : Fuzzer Msg
--hover =
--    Fuzz.oneOf
--        [ Fuzz.constant NoHover
--        , Fuzz.map HoverLine Fuzz.int
--        , Fuzz.map2 (\line column -> HoverChar { line = line, column = column }) Fuzz.int Fuzz.int
--        ]
--        |> Fuzz.map Hover
--
--
--goToHoveredPosition : Fuzzer Msg
--goToHoveredPosition =
--    Fuzz.constant GoToHoveredPosition
