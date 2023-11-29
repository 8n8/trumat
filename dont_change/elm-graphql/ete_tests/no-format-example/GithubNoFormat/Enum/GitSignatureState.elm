-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Enum.GitSignatureState exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The state of a Git signature.

  - Valid - Valid signature and verified by GitHub.
  - Invalid - Invalid signature.
  - MalformedSig - Malformed signature.
  - UnknownKey - Key used for signing not known to GitHub.
  - BadEmail - Invalid email used for signing.
  - UnverifiedEmail - Email used for signing unverified on GitHub.
  - NoUser - Email used for signing not known to GitHub.
  - UnknownSigType - Unknown signature type.
  - Unsigned - Unsigned.
  - GpgverifyUnavailable - Internal error - the GPG verification service is unavailable at the moment.
  - GpgverifyError - Internal error - the GPG verification service misbehaved.
  - NotSigningKey - The usage flags for the key that signed this don't allow signing.
  - ExpiredKey - Signing key expired.

-}
type GitSignatureState
    = Valid
    | Invalid
    | MalformedSig
    | UnknownKey
    | BadEmail
    | UnverifiedEmail
    | NoUser
    | UnknownSigType
    | Unsigned
    | GpgverifyUnavailable
    | GpgverifyError
    | NotSigningKey
    | ExpiredKey


list : List GitSignatureState
list =
    [ Valid, Invalid, MalformedSig, UnknownKey, BadEmail, UnverifiedEmail, NoUser, UnknownSigType, Unsigned, GpgverifyUnavailable, GpgverifyError, NotSigningKey, ExpiredKey ]


decoder : Decoder GitSignatureState
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "VALID" ->
                        Decode.succeed Valid

                    "INVALID" ->
                        Decode.succeed Invalid

                    "MALFORMED_SIG" ->
                        Decode.succeed MalformedSig

                    "UNKNOWN_KEY" ->
                        Decode.succeed UnknownKey

                    "BAD_EMAIL" ->
                        Decode.succeed BadEmail

                    "UNVERIFIED_EMAIL" ->
                        Decode.succeed UnverifiedEmail

                    "NO_USER" ->
                        Decode.succeed NoUser

                    "UNKNOWN_SIG_TYPE" ->
                        Decode.succeed UnknownSigType

                    "UNSIGNED" ->
                        Decode.succeed Unsigned

                    "GPGVERIFY_UNAVAILABLE" ->
                        Decode.succeed GpgverifyUnavailable

                    "GPGVERIFY_ERROR" ->
                        Decode.succeed GpgverifyError

                    "NOT_SIGNING_KEY" ->
                        Decode.succeed NotSigningKey

                    "EXPIRED_KEY" ->
                        Decode.succeed ExpiredKey

                    _ ->
                        Decode.fail ("Invalid GitSignatureState type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : GitSignatureState -> String
toString enum____ =
    case enum____ of
        Valid ->
            "VALID"

        Invalid ->
            "INVALID"

        MalformedSig ->
            "MALFORMED_SIG"

        UnknownKey ->
            "UNKNOWN_KEY"

        BadEmail ->
            "BAD_EMAIL"

        UnverifiedEmail ->
            "UNVERIFIED_EMAIL"

        NoUser ->
            "NO_USER"

        UnknownSigType ->
            "UNKNOWN_SIG_TYPE"

        Unsigned ->
            "UNSIGNED"

        GpgverifyUnavailable ->
            "GPGVERIFY_UNAVAILABLE"

        GpgverifyError ->
            "GPGVERIFY_ERROR"

        NotSigningKey ->
            "NOT_SIGNING_KEY"

        ExpiredKey ->
            "EXPIRED_KEY"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe GitSignatureState
fromString enumString____ =
    case enumString____ of
        "VALID" ->
            Just Valid

        "INVALID" ->
            Just Invalid

        "MALFORMED_SIG" ->
            Just MalformedSig

        "UNKNOWN_KEY" ->
            Just UnknownKey

        "BAD_EMAIL" ->
            Just BadEmail

        "UNVERIFIED_EMAIL" ->
            Just UnverifiedEmail

        "NO_USER" ->
            Just NoUser

        "UNKNOWN_SIG_TYPE" ->
            Just UnknownSigType

        "UNSIGNED" ->
            Just Unsigned

        "GPGVERIFY_UNAVAILABLE" ->
            Just GpgverifyUnavailable

        "GPGVERIFY_ERROR" ->
            Just GpgverifyError

        "NOT_SIGNING_KEY" ->
            Just NotSigningKey

        "EXPIRED_KEY" ->
            Just ExpiredKey

        _ ->
            Nothing
