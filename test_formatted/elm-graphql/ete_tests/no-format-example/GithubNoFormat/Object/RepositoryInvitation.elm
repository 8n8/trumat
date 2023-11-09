-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.RepositoryInvitation exposing (..)

import GithubNoFormat.Enum.RepositoryPermission
import GithubNoFormat.InputObject
import GithubNoFormat.Interface
import GithubNoFormat.Object
import GithubNoFormat.Scalar
import GithubNoFormat.ScalarCodecs
import GithubNoFormat.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.RepositoryInvitation
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The user who received the invitation.
-}
invitee :
    SelectionSet decodesTo GithubNoFormat.Object.User
    -> SelectionSet decodesTo GithubNoFormat.Object.RepositoryInvitation
invitee object____ =
    Object.selectionForCompositeField "invitee" [] object____ Basics.identity


{-| The user who created the invitation.
-}
inviter :
    SelectionSet decodesTo GithubNoFormat.Object.User
    -> SelectionSet decodesTo GithubNoFormat.Object.RepositoryInvitation
inviter object____ =
    Object.selectionForCompositeField "inviter" [] object____ Basics.identity


{-| The permission granted on this repository by this invitation.
-}
permission : SelectionSet GithubNoFormat.Enum.RepositoryPermission.RepositoryPermission GithubNoFormat.Object.RepositoryInvitation
permission =
    Object.selectionForField "Enum.RepositoryPermission.RepositoryPermission" "permission" [] GithubNoFormat.Enum.RepositoryPermission.decoder


{-| The Repository the user is invited to.
-}
repository :
    SelectionSet decodesTo GithubNoFormat.Object.RepositoryInvitationRepository
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.RepositoryInvitation
repository object____ =
    Object.selectionForCompositeField "repository" [] object____ (Basics.identity >> Decode.nullable)
