# ded

*Note that this project is a work in progress and doesn't work yet.*

Ded is a fast and opinionated linter for Elm-lang. It takes effectively zero time to run from a human point of view, even on a large codebase.

It only bothers with things it can fix automatically, so it never prints out warnings. It just over-writes the source.

It does these three things:

1. removes unused code
2. formats the code in the same style as `elm-format`
3. adds type signatures for all the variables that don't have them, including in `let`-`in` expressions
