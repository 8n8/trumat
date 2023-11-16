module BufferComparison exposing (suite)

import Array
import Array.Util
import Buffer as Buffer1
import Buffer2
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Position exposing (Position)
import Test exposing (..)


suite : Test
suite =
    describe "Compare Buffer implementations"
        [ describe "Basic operations"
            [ test "init" <|
                \_ ->
                    let
                        str =
                            "one\ntwo"

                        buffer1 =
                            Buffer1.init str
                                |> Buffer1.toString
                    in
                    str
                        |> Buffer2.init
                        |> Buffer2.toString
                        |> Expect.equal buffer1
            , test "insert one character" <|
                \_ ->
                    let
                        str =
                            "aaa\nbbb"

                        buffer1 =
                            str
                                |> Buffer1.init
                                |> Buffer1.insert (Position 0 1) "X"
                                |> Buffer1.toString

                        buffer2 =
                            str
                                |> Buffer2.init
                                |> Buffer2.insert (Position 0 1) "X"
                                |> Buffer2.toString
                    in
                    buffer1
                        |> Expect.equal buffer2
            , test "insertion of one character with Buffer2 is valid " <|
                \_ ->
                    let
                        str =
                            "aaa\nbbb"

                        buffer2 =
                            str
                                |> Buffer2.init
                                |> Buffer2.insert (Position 0 1) "X"
                    in
                    Buffer2.valid buffer2
                        |> Expect.equal True
            , test "insert newline" <|
                \_ ->
                    let
                        str =
                            "aaa\nbbb"

                        buffer1 =
                            str
                                |> Buffer1.init
                                |> Buffer1.insert (Position 0 1) "\n"
                                |> Buffer1.toString
                                |> Debug.log "1"

                        buffer2 =
                            str
                                |> Buffer2.init
                                |> Buffer2.insert (Position 0 1) "\n"
                                |> Buffer2.toString
                                |> Debug.log "2"
                    in
                    buffer1
                        |> Expect.equal buffer2
            , test "insertion of newline with Buffer2 is valid " <|
                \_ ->
                    let
                        str =
                            "aaa\nbbb"

                        buffer2 =
                            str
                                |> Buffer2.init
                                |> Buffer2.insert (Position 0 1) "\n"
                    in
                    Buffer2.valid buffer2
                        |> Expect.equal True
            ]
        ]
