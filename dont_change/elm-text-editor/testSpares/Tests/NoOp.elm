module Tests.NoOp exposing (doesNothing)

import ArchitectureTest exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import Tests.Common exposing (..)


type alias MsgTestO model msg =
    String
    -> TestedApp model msg
    -> Fuzzer msg
    -> (model -> msg -> model -> Expectation)
    -> Test


doesNothing : Test
doesNothing =
    msgTest "NoOp does nothing" app noOp <|
        \modelBeforeMsg _ finalModel ->
            modelBeforeMsg
                |> Expect.equal finalModel
