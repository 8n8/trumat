module Buffer2Test exposing (suite)

import Array
import Array.Util
import Buffer2 as Buffer exposing (Buffer(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Position exposing (Position)
import Test exposing (..)


suite : Test
suite =
    describe "Buffer"
        [ describe "init"
            [ test "Buffer.init creates a buffer  is properly initialized" <|
                \_ ->
                    let
                        buffer =
                            Buffer.init "one\ntwo"
                    in
                    Expect.equal buffer (Buffer { content = "one\ntwo", lines = Array.fromList [ "one", "two" ] })
            , test "Buffer created by init is valid" <|
                \_ ->
                    Buffer.init "one\ntwo"
                        |> Buffer.valid
                        |> Expect.equal True
            , test "Replace a string in the buffer" <|
                \_ ->
                    Buffer.init "One\ntwo\nthree\nfour"
                        |> Buffer.replace (Position 1 0) (Position 1 3) "TWO"
                        |> Expect.equal (Buffer { content = "One\nTWO\nthree\nfour", lines = Array.fromList [ "One", "TWO", "three", "four" ] })
            ]
        , describe "Array.Util"
            [ test "insert (1)" <|
                \_ ->
                    Array.fromList [ "aaa", "bbb" ]
                        |> Array.Util.insert (Position 0 1) "X"
                        |> Expect.equal (Array.fromList [ "aXaa", "bbb" ])
            , test "split" <|
                \_ ->
                    Array.fromList [ "aaa", "xyz", "ccc" ]
                        |> Array.Util.split (Position 1 1)
                        |> Expect.equal (Array.fromList [ "aaa", "x", "yz", "xyz" ])
            , test "cut" <|
                \_ ->
                    let
                        result =
                            { before = Array.fromList [ "abcde", "f" ]
                            , middle = Array.fromList [ "ghij", "k" ]
                            , after = Array.fromList [ "lmn", "opqr" ]
                            }
                    in
                    Array.fromList [ "abcde", "fghij", "klmn", "opqr" ]
                        |> Array.Util.cut (Position 1 1) (Position 2 1)
                        |> Expect.equal result
            , test "cutOut" <|
                \_ ->
                    Array.fromList [ "abcde", "fghij", "klmn", "opqr" ]
                        |> Array.Util.cutOut (Position 1 1) (Position 2 1)
                        |> Expect.equal ( Array.fromList [ "ghij", "k" ], Array.fromList [ "abcde", "f", "lmn", "opqr" ] )
            , test "replace" <|
                \_ ->
                    Array.fromList [ "abcde", "fghij", "klmn", "opqr" ]
                        |> Array.Util.replace (Position 1 1) (Position 2 1) "UVW"
                        |> Expect.equal (Array.fromList [ "abcde", "f", "UVW", "lmn", "opqr" ])
            ]
        ]
