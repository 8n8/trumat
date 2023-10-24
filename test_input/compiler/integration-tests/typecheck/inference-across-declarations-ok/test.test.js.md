# Snapshot report for `integration-tests/typecheck/inference-across-declarations-ok/test.test.js`

The actual snapshot is saved in `test.test.js.snap`.

Generated by [AVA](https://avajs.dev).

## Invocation

    'elm-in-elm -m src/Main.elm'

## Stderr

    `Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.␊
    `

## Stdout

    `---------------------------␊
    -- STARTING THE COMPILER --␊
    ---------------------------␊
    Main.someFunction: Value { expression = Lambda { arguments = ["a","b"], body = Plus (Int 3) (Argument "a") }, typeAnnotation = Just { type_ = Function { from = Int, to = Function { from = TypeVar "a", to = Int } }, varName = "someFunction" } }␊
    Main.main: Value { expression = Call { argument = Int 5, fn = Var { name = "someFunction", qualifiedness = PossiblyQualified Nothing } }, typeAnnotation = Just { type_ = Function { from = TypeVar "a", to = Int }, varName = "main" } }␊
    Compilation finished, writing output.␊
    ---------------------------␊
    -- WRITING TO FS ----------␊
    ---------------------------␊
    const Main$someFunction = ((a) => ((b) => (3 + a)));␊
    const Main$main = (Main$someFunction(5));␊
    `

## out.js

    `const Main$someFunction = ((a) => ((b) => (3 + a)));␊
    const Main$main = (Main$someFunction(5));`