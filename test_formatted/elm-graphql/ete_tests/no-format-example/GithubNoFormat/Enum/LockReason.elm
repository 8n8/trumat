-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Enum.LockReason exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The possible reasons that an issue or pull request was locked.

  - OffTopic - The issue or pull request was locked because the conversation was off-topic.
  - TooHeated - The issue or pull request was locked because the conversation was too heated.
  - Resolved - The issue or pull request was locked because the conversation was resolved.
  - Spam - The issue or pull request was locked because the conversation was spam.

-}
type LockReason
    = OffTopic
    | TooHeated
    | Resolved
    | Spam


list : List LockReason
list =
    [ OffTopic, TooHeated, Resolved, Spam ]


decoder : Decoder LockReason
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "OFF_TOPIC" ->
                        Decode.succeed OffTopic

                    "TOO_HEATED" ->
                        Decode.succeed TooHeated

                    "RESOLVED" ->
                        Decode.succeed Resolved

                    "SPAM" ->
                        Decode.succeed Spam

                    _ ->
                        Decode.fail ("Invalid LockReason type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : LockReason -> String
toString enum____ =
    case enum____ of
        OffTopic ->
            "OFF_TOPIC"

        TooHeated ->
            "TOO_HEATED"

        Resolved ->
            "RESOLVED"

        Spam ->
            "SPAM"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe LockReason
fromString enumString____ =
    case enumString____ of
        "OFF_TOPIC" ->
            Just OffTopic

        "TOO_HEATED" ->
            Just TooHeated

        "RESOLVED" ->
            Just Resolved

        "SPAM" ->
            Just Spam

        _ ->
            Nothing
