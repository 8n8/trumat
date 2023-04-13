module Tests exposing (suite)

import Expect
import Format exposing (format)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Formatter" (List.map oneTest cases)


oneTest :
    { description : String, input : String, expected : String }
    -> Test
oneTest { description, input, expected } =
    test description <|
        \_ ->
            Expect.equal (Ok expected) (format input)


cases : List { description : String, input : String, expected : String }
cases =
    [ { description = "Hello world formatted"
      , input = """module X exposing (x)


x =
    0
"""
      , expected = """module X exposing (x)


x =
    0
"""
      }
    ]
