module Format exposing (format)

import Array exposing (Array)


format : String -> Result String String
format raw =
    String.toList raw
        |> Array.fromList
        |> formatHelp "" 0 (ModuleKeyword { numConsumed = 0, remainder = "module " })


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

        Finish ->
            Ok accumulator

        Fail error ->
            Err error


type Action
    = MoveBackBy Int
    | Commit String
    | Finish
    | Fail String


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
    = ModuleKeyword ChunkMachine
    | TopLevelBindName
    | SpaceAfterModuleKeyword


machine : Token -> State -> ( State, Action )
machine token state =
    case (token, state) of
        (AfterEnd, TopLevelBindName) ->
            (state, Fail "unexpected end of file while parsing top-level bind name")
        (BeforeStart, TopLevelBindName) ->
            (state, Fail "internal error: (BeforeStart, TopLevelBindName)")
        (DontCare _, TopLevelBindName) ->
            (state, Fail "unexpected character in top-level bind name")
        (_, ModuleKeyword oldChunkState) ->
            case chunkMachine token oldChunkState of
                FinishedChunk ->
                    (SpaceAfterModuleKeyword, Commit "module ")

                ContinueChunk newChunkState ->
                    (ModuleKeyword newChunkState, Commit "")

                FailedChunk backtrack ->
                    (TopLevelBindName, MoveBackBy backtrack)
        (_, TopLevelBindName) ->
            case ordinaryNameMachine token of
                FinishedName ->
                    (SpaceAfter
            

        

type ChunkResult
    = FinishedChunk
    | ContinueChunk ChunkMachine
    | FailedChunk Int

type alias ChunkMachine =
    { numConsumed : Int
    , remainder : String
    }

chunkMachine : Token -> ChunkMachine -> ChunkResult
chunkMachine token {numConsumed, remainder} =
    case String.uncons remainder of
        Just (top, tail) ->
            if tokenize top == token then
                ContinueChunk
                    { numConsumed = numConsumed + 1
                    , remainder = tail
                    }

            else
                FailedChunk numConsumed

        Nothing ->
            FinishedChunk
