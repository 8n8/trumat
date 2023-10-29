-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.ProjectState exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| State of the project; either 'open' or 'closed'

  - Open - The project is open.
  - Closed - The project is closed.

-}
type ProjectState
    = Open
    | Closed


list : List ProjectState
list =
    [ Open, Closed ]


decoder : Decoder ProjectState
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "OPEN" ->
                        Decode.succeed Open

                    "CLOSED" ->
                        Decode.succeed Closed

                    _ ->
                        Decode.fail ("Invalid ProjectState type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProjectState -> String
toString enum____ =
    case enum____ of
        Open ->
            "OPEN"

        Closed ->
            "CLOSED"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProjectState
fromString enumString____ =
    case enumString____ of
        "OPEN" ->
            Just Open

        "CLOSED" ->
            Just Closed

        _ ->
            Nothing
