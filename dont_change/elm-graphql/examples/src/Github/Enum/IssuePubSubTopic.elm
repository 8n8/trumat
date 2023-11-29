-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Enum.IssuePubSubTopic exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The possible PubSub channels for an issue.

  - Updated - The channel ID for observing issue updates.
  - Markasread - The channel ID for marking an issue as read.

-}
type IssuePubSubTopic
    = Updated
    | Markasread


list : List IssuePubSubTopic
list =
    [ Updated, Markasread ]


decoder : Decoder IssuePubSubTopic
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "UPDATED" ->
                        Decode.succeed Updated

                    "MARKASREAD" ->
                        Decode.succeed Markasread

                    _ ->
                        Decode.fail ("Invalid IssuePubSubTopic type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : IssuePubSubTopic -> String
toString enum____ =
    case enum____ of
        Updated ->
            "UPDATED"

        Markasread ->
            "MARKASREAD"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe IssuePubSubTopic
fromString enumString____ =
    case enumString____ of
        "UPDATED" ->
            Just Updated

        "MARKASREAD" ->
            Just Markasread

        _ ->
            Nothing
