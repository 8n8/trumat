import main

minimal_with_extra_variable_hi = {
    "elm.json": """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
""",
    "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hi"


a = 0
""",
}


minimal_without_extra_variable_hi = {
    "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hi"
"""
}


minimal_with_extra_variable_hello = {
    "elm.json": """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
""",
    "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hello"


a = 0
""",
}


minimal_without_extra_variable_hello = {
    "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hello"
"""
}


def test_simple_hi():
    expected = minimal_without_extra_variable_hi
    got = main.lint(minimal_with_extra_variable_hi)
    assert expected == got


def test_simple_hello():
    expected = minimal_without_extra_variable_hello
    got = main.lint(minimal_with_extra_variable_hello)
    assert expected == got


minimal_nothing_to_do = {
    "elm.json": """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
""",
    "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hello"
""",
}


def test_nothing_to_do():
    assert main.lint(minimal_nothing_to_do) == {}
