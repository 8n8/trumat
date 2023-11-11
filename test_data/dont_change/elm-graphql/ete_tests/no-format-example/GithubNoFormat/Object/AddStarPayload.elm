-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.AddStarPayload exposing (..)

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


{-| A unique identifier for the client performing the mutation.
-}
clientMutationId : SelectionSet (Maybe String) GithubNoFormat.Object.AddStarPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The starrable.
-}
starrable :
    SelectionSet decodesTo GithubNoFormat.Interface.Starrable
    -> SelectionSet decodesTo GithubNoFormat.Object.AddStarPayload
starrable object____ =
    Object.selectionForCompositeField "starrable" [] object____ Basics.identity