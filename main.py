def lint(unlinted):
    return {
        "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hi"
"""
    }
