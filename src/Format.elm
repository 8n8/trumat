module Format exposing (format)

import Array exposing (Array)


format : String -> Result String String
format raw =
    String.toList raw
        |> Array.fromList
        |> formatHelp "" 0 Start


formatHelp : String -> Int -> State -> Array Char -> Result String String
formatHelp accumulator index oldState chars =
    let
        token =
            case Array.get index chars of
                Nothing ->
                    if index < 0 then
                        BeforeStart

                    else
                        AfterEnd

                Just char ->
                    tokenize char

        ( newState, action ) =
            machine token oldState
    in
    case action of
        MoveBackBy distance ->
            formatHelp accumulator (index - distance) newState chars

        Commit formatted ->
            formatHelp (accumulator ++ formatted) (index + 1) newState chars

        Fail error ->
            Err error

        Finish ->
            Ok accumulator


type Action
    = MoveBackBy Int
    | Commit String
    | Fail String
    | Finish


type Token
    = Lowercase Alphabet
    | DontCare Char
    | BeforeStart
    | AfterEnd


type Alphabet
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z


tokenize : Char -> Token
tokenize char =
    case char of
        'm' ->
            Lowercase M

        _ ->
            DontCare char


type State
    = Start


machine : Token -> State -> ( State, Action )
machine token state =
    ( state, Finish )
