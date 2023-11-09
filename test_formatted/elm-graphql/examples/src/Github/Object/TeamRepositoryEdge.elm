-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.TeamRepositoryEdge exposing (..)

import Github.Enum.RepositoryPermission
import Github.InputObject
import Github.Interface
import Github.Object
import Github.Scalar
import Github.ScalarCodecs
import Github.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


cursor : SelectionSet String Github.Object.TeamRepositoryEdge
cursor =
    Object.selectionForField "String" "cursor" [] Decode.string


node :
    SelectionSet decodesTo Github.Object.Repository
    -> SelectionSet decodesTo Github.Object.TeamRepositoryEdge
node object____ =
    Object.selectionForCompositeField "node" [] object____ Basics.identity


{-| The permission level the team has on the repository
-}
permission : SelectionSet Github.Enum.RepositoryPermission.RepositoryPermission Github.Object.TeamRepositoryEdge
permission =
    Object.selectionForField "Enum.RepositoryPermission.RepositoryPermission" "permission" [] Github.Enum.RepositoryPermission.decoder
