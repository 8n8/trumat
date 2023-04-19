module Format exposing (format)

import Array exposing (Array)


format : String -> Result String String
format raw =
    String.toList raw
        |> Array.fromList
        |> formatHelp "" 0 (ModuleKeyword (InKeyword { numConsumed = 0, remainder = "module " }))


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
                    Unicode char

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

        Batch batch ->
            formatHelp accumulator index newState chars


type Action
    = MoveBackBy Int
    | Commit String
    | Finish
    | Fail String
    | Batch (List Action)


type Token
    = Unicode Char
    | BeforeStart
    | AfterEnd


type State
    = ModuleKeyword KeywordMachine
    | TopLevelBindName NameMachine
    | SpaceAfterModuleKeyword SpaceMachine
    | SpaceAfterTopLevelBind SpaceMachine
    | ModuleExposingKeyword KeywordMachine
    | EqualsAfterTopLevelBind
    | SpaceAfterModuleExposingKeyword SpaceMachine
    | SpaceBeforeTopLevelExpression SpaceMachine
    | Exports ChunkMachine


type SpaceMachine
    = OpenCurly
    | HyphenAfterCurly
    | HyphenBeforeCurly
    | InBlock
    | InDoc
    | FirstLineCommentHyphen
    | NotInComment


type SpaceResult
    = FinishedSpace
    | ContinueSpace SpaceMachine Char


someSpaceMachine : Token -> SpaceMachine -> SpaceResult
someSpaceMachine token state =
    case (token, state) of
        (Unicode char, OpenCurly) ->
            if char == '-' then
                ContinueSpace HyphenAfterCurly char

            else
                FinishedSpace


machine : Token -> State -> ( State, Action )
machine token state =
    case (token, state) of
        (AfterEnd, TopLevelBindName _) ->
            (state, Fail "unexpected end of file while parsing top-level bind name")
        (BeforeStart, TopLevelBindName _) ->
            (state, Fail "internal error: (BeforeStart, TopLevelBindName)")
        (_, ModuleKeyword oldKeywordState) ->
            case keywordMachine token oldKeywordState of
                FinishedKeyword ->
                    (SpaceAfterModuleKeyword NotInComment, Commit "module ")

                ContinueKeyword newKeywordState ->
                    (ModuleKeyword newKeywordState, Commit "")

                FailedKeyword backtrack ->
                    (TopLevelBindName FirstNameChar, MoveBackBy backtrack)
        (_, TopLevelBindName oldMachine) ->
            case ordinaryNameMachine token oldMachine of
                FinishedName ->
                    (SpaceAfterTopLevelBind NotInComment, MoveBackBy 1)

                ContinueName newMachine commit ->
                    (TopLevelBindName newMachine, Commit (String.fromChar commit))

                FailedName ->
                    (state, Fail "could not parse top-level bind name")

        ( _, SpaceAfterModuleKeyword oldMachine) ->
            case someSpaceMachine token oldMachine of
                FinishedSpace ->
                    ( {numConsumed = 0, remainder = "exposing" }
                        |> InKeyword
                        |> ModuleExposingKeyword
                    , MoveBackBy 1
                    )

                ContinueSpace newMachine commit ->
                    ( SpaceAfterModuleKeyword newMachine
                    , Commit (String.fromChar commit)
                    )

        ( _, SpaceAfterTopLevelBind oldMachine ) ->
            case someSpaceMachine token oldMachine of
                FinishedSpace ->
                    (EqualsAfterTopLevelBind, MoveBackBy 1)

                ContinueSpace newMachine commit ->
                    ( SpaceAfterTopLevelBind newMachine
                    , Commit (String.fromChar commit)
                    )

        ( _, ModuleExposingKeyword oldMachine) ->
            case keywordMachine token oldMachine of
                FinishedKeyword ->
                    (SpaceAfterModuleExposingKeyword NotInComment, Commit "exposing")

                ContinueKeyword newMachine ->
                    (ModuleExposingKeyword newMachine, Commit "")

                FailedKeyword backtrack ->
                    (state, Fail "expecting 'exposing' keyword")

        (Unicode char, EqualsAfterTopLevelBind) ->
            if char == '=' then
                (SpaceBeforeTopLevelExpression NotInComment , Commit "=")

            else
                (state, Fail "expected '=' after top-level bind")

        (_, SpaceAfterModuleExposingKeyword oldState) ->
            case someSpaceMachine token oldState of
                FinishedSpace ->
                    ( Exports {numConsumed = 0, remainder = "(x)"}
                    , Batch [Commit " ", MoveBackBy 1]
                    )

                ContinueSpace newState char ->
                    ( SpaceAfterModuleExposingKeyword newState
                    , Commit (String.fromChar char)
                    )


isEndNameChar : Char -> Bool
isEndNameChar char =
    case char of
        ')' ->
            True

        '=' ->
            True

        ']' ->
            True

        _ ->
            False


isSubsequentNameChar : Char -> Bool
isSubsequentNameChar char =
    case char of
        '0' -> True
        '1' -> True
        '2' -> True
        '3' -> True
        '4' -> True
        '5' -> True
        '6' -> True
        '7' -> True
        '8' -> True
        '9' -> True
        '_' -> True
        'A' -> True
        'B' -> True
        'C' -> True
        'D' -> True
        'E' -> True
        'F' -> True
        'G' -> True
        'H' -> True
        'I' -> True
        'J' -> True
        'K' -> True
        'L' -> True
        'M' -> True
        'N' -> True
        'O' -> True
        'P' -> True
        'Q' -> True
        'R' -> True
        'S' -> True
        'T' -> True
        'U' -> True
        'V' -> True
        'W' -> True
        'X' -> True
        'Y' -> True
        'Z' -> True
        'a' -> True
        'b' -> True
        'c' -> True
        'd' -> True
        'e' -> True
        'f' -> True
        'g' -> True
        'h' -> True
        'i' -> True
        'j' -> True
        'k' -> True
        'l' -> True
        'm' -> True
        'n' -> True
        'o' -> True
        'p' -> True
        'q' -> True
        'r' -> True
        's' -> True
        't' -> True
        'u' -> True
        'v' -> True
        'w' -> True
        'x' -> True
        'y' -> True
        'z' -> True


type NameMachine
    = FirstNameChar
    | SubsequentNameChar



ordinaryNameMachine : Token -> NameMachine -> NameResult
ordinaryNameMachine token state =
    case (token, state) of
        (Unicode char, SubsequentNameChar) ->
            if isSubsequentNameChar char then
                ContinueName SubsequentNameChar char

            else if isEndNameChar char then
                FinishedName

            else
                FailedName

        (BeforeStart, SubsequentNameChar) ->
            FailedName

        (AfterEnd, SubsequentNameChar) ->
            FinishedName

type NameResult
    = FinishedName
    | ContinueName NameMachine Char
    | FailedName

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
            if (Unicode top) == token then
                ContinueChunk
                    { numConsumed = numConsumed + 1
                    , remainder = tail
                    }

            else
                FailedChunk numConsumed

        Nothing ->
            FinishedChunk

type KeywordMachine
    = InKeyword { numConsumed : Int, remainder : String }
    | AfterKeyword

type KeywordResult
    = FinishedKeyword
    | ContinueKeyword KeywordMachine
    | FailedKeyword Int

keywordMachine : Token -> KeywordMachine -> KeywordResult
keywordMachine token state =
    case state of
        InKeyword {numConsumed, remainder} ->
            case String.uncons remainder of
                Just (top, tail) ->
                    if (Unicode top) == token then
                            { numConsumed = numConsumed + 1
                            , remainder = tail
                            }
                                |> InKeyword
                                |> ContinueKeyword

                    else
                        FailedKeyword numConsumed
