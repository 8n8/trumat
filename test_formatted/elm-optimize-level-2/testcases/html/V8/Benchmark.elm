port module V8.Benchmark exposing (main)

{-| -}

import Json.Encode
import Suite
import V8.Benchmark.Runner.Json


main : V8.Benchmark.Runner.Json.JsonBenchmark
main =
    V8.Benchmark.Runner.Json.program
        reportResults
        Suite.suite


port reportResults : Json.Encode.Value -> Cmd msg
