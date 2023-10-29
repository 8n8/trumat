module DemoSimple exposing (Msg(..), main)

import AppText
import Browser
import Editor exposing (Editor, EditorConfig, EditorMsg)
import Editor.Config exposing (WrapOption(..))
import Editor.Strings
import Html exposing (Html, button, div, text)
import Html.Attributes as HA exposing (style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type Msg
    = EditorMsg EditorMsg
    | Test
    | FindTreasure
    | GetSpeech
    | Get500
    | Get1000
    | Get1500
    | Get3000
    | Jabberwocky
    | Code
    | About
    | LogErr String


type alias Model =
    { editor : Editor
    , clipboard : String
    , document : Document
    }


type Document
    = DocJabberWock
    | DocGettysburg
    | DocLongLines
    | Doc500
    | Doc1000
    | Doc1500
    | Doc3000
    | DocAbout
    | DocCode


init : () -> ( Model, Cmd Msg )
init () =
    ( { editor = Editor.init config AppText.about
      , clipboard = ""
      , document = DocAbout
      }
    , Cmd.none
    )


config : EditorConfig Msg
config =
    { editorMsg = EditorMsg
    , width = 500
    , height = 480
    , lineHeight = 16.0
    , showInfoPanel = True
    , wrapParams = { maximumWidth = 55, optimalWidth = 50, stringWidth = String.length }
    , wrapOption = DontWrap
    , fontProportion = 0.75
    , lineHeightFactor = 1.0
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( editor, cmd ) =
                    Editor.update editorMsg model.editor
            in
            ( { model | editor = editor }, Cmd.map EditorMsg cmd )

        Test ->
            load DontWrap Editor.Strings.info model

        GetSpeech ->
            load DoWrap AppText.gettysburgAddress { model | document = DocGettysburg }

        Get500 ->
            load DontWrap AppText.words500 { model | document = Doc500 }

        Get1000 ->
            load DontWrap AppText.words1000 { model | document = Doc1000 }

        Get1500 ->
            load DontWrap AppText.words1500 { model | document = Doc1500 }

        Get3000 ->
            load DontWrap AppText.words3000 { model | document = Doc3000 }

        Jabberwocky ->
            load DontWrap AppText.jabberwocky { model | document = DocJabberWock }

        About ->
            load DontWrap AppText.about { model | document = DocAbout }

        Code ->
            load DontWrap AppText.code { model | document = DocCode }

        FindTreasure ->
            highlightText "treasure" model

        LogErr _ ->
            ( model, Cmd.none )



-- HELPER FUNCTIONS FOR UPDATE


{-| Paste contents of clipboard into Editor
-}
pasteToClipboard : Model -> String -> ( Model, Cmd msg )
pasteToClipboard model editor =
    ( { model
        | editor =
            Editor.insert
                (Editor.getWrapOption model.editor)
                (Editor.getCursor model.editor)
                editor
                model.editor
      }
    , Cmd.none
    )


pasteToEditorClipboard : Model -> String -> ( Model, Cmd msg )
pasteToEditorClipboard model str =
    let
        cursor =
            Editor.getCursor model.editor

        wrapOption =
            Editor.getWrapOption model.editor

        editor2 =
            Editor.placeInClipboard str model.editor
    in
    ( { model | editor = Editor.insert wrapOption cursor str editor2 }, Cmd.none )


{-| Load text into Editor
-}
load : WrapOption -> String -> Model -> ( Model, Cmd Msg )
load wrapOption text model =
    let
        newEditor =
            Editor.load wrapOption text model.editor
    in
    ( { model | editor = newEditor }, Cmd.none )


{-| Find str and highlight it
-}
highlightText : String -> Model -> ( Model, Cmd Msg )
highlightText str model =
    let
        newEditor =
            Editor.scrollToString str model.editor
    in
    ( { model | editor = newEditor }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ HA.style "margin" "60px"
        ]
        [ title
        , div [ HA.style "width" (px <| Editor.getWidth model.editor) ] [ Editor.embedded config model.editor ]
        , footer model
        ]


px : Float -> String
px p =
    String.fromFloat p ++ "px"


title : Html Msg
title =
    div
        [ HA.style "font-size" "16px"
        , HA.style "font-style" "bold"
        , HA.style "margin-bottom" "10px"
        ]
        [ text "A Pure Elm Text Editor (Simple)" ]


footer : Model -> Html Msg
footer model =
    div
        [ HA.style "font-size" "14px", HA.style "position" "absolute", HA.style "top" "590px", HA.style "left" "80px" ]
        [ div []
            [ Html.a [ HA.href "https://github.com/jxxcarlson/elm-text-editor" ] [ text "Source code (Work in Progress)" ]
            ]
        , div [ HA.style "margin-top" "10px" ]
            [ text "This editor is based on  "
            , Html.a [ HA.href "https://sidneynemzer.github.io/elm-text-editor/" ]
                [ text "work of Sydney Nemzer" ]
            , Html.span [] [ text " and is inspired by previous work of " ]
            , Html.a [ HA.href "https://discourse.elm-lang.org/t/text-editor-done-in-pure-elm/1365" ] [ text "Martin Janiczek" ]
            ]
        , div [ HA.style "margin-top" "10px" ] [ text "ctrl-h to toggle help, ctrl-shift-w to wrap all text" ]
        , div [ HA.style "margin-top" "10px" ]
            [ aboutButton model
            , codeButton model
            , jabberWockyButton model
            , speechTextButton model
            , textButton500 model
            , textButton1000 model
            , textButton1500 model
            , textButton3000 model
            ]
        ]



-- BUTTONS


speechTextButton model =
    rowButton model 160 GetSpeech DocGettysburg "Gettysburg Address" []


textButton500 model =
    rowButton model 100 Get500 Doc500 "500 words" []


textButton1000 model =
    rowButton model 100 Get1000 Doc1000 "1000 words" []


textButton1500 model =
    rowButton model 100 Get1500 Doc1500 "1500 words" []


textButton3000 model =
    rowButton model 100 Get3000 Doc3000 "3000 words" []


jabberWockyButton model =
    rowButton model 100 Jabberwocky DocJabberWock "Jabberwocky" []


aboutButton model =
    rowButton model 80 About DocAbout "About" []


codeButton model =
    rowButton model 80 Code DocCode "Code" []


highlight : b -> b -> List (Html.Attribute msg)
highlight source target =
    case source == target of
        True ->
            [ style "background-color" "#900" ]

        False ->
            [ style "background-color" "#666" ]



-- STYLE --


rowButtonStyle =
    [ style "font-size" "12px"
    , style "border" "none"
    , style "margin-right" "8px"
    , style "float" "left"
    ]


rowButtonLabelStyle width =
    [ style "font-size" "12px"
    , style "color" "#eee"
    , style "width" (String.fromInt width ++ "px")
    , style "height" "24px"
    , style "border" "none"
    ]



-- rowButton : Model -> Int -> b -> Document -> String -> List (Html.Attribute msg) -> Html Msg


rowButton model width msg doc str attr =
    div (rowButtonStyle ++ attr)
        [ button ([ onClick msg ] ++ rowButtonLabelStyle width ++ highlight doc model.document) [ text str ] ]
