def up_to_double_quote(src, pos):
    i = 0
    for i, char in enumerate(src[pos:]):
        if char == '"':
            break

    return src[pos : i + pos], i + pos


def lint(unlinted):
    _, i = up_to_double_quote(unlinted["src/Main.elm"], 0)
    quoted, _ = up_to_double_quote(unlinted["src/Main.elm"], i + 1)
    print("quoted:", quoted)
    return {
        "src/Main.elm": f"""module Main exposing (main)

import Html

main =
    Html.text "{quoted}"
"""
    }
