-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.ReviewDismissalAllowanceEdge exposing (..)

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


{-| A cursor for use in pagination.
-}
cursor : SelectionSet String GithubNoFormat.Object.ReviewDismissalAllowanceEdge
cursor =
    Object.selectionForField "String" "cursor" [] Decode.string


{-| The item at the end of the edge.
-}
node :
    SelectionSet decodesTo GithubNoFormat.Object.ReviewDismissalAllowance
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.ReviewDismissalAllowanceEdge
node object____ =
    Object.selectionForCompositeField "node" [] object____ (Basics.identity >> Decode.nullable)
