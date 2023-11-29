-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.ReleaseAssetConnection exposing (..)

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


{-| A list of edges.
-}
edges :
    SelectionSet decodesTo GithubNoFormat.Object.ReleaseAssetEdge
    -> SelectionSet (Maybe (List (Maybe decodesTo))) GithubNoFormat.Object.ReleaseAssetConnection
edges object____ =
    Object.selectionForCompositeField "edges" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| A list of nodes.
-}
nodes :
    SelectionSet decodesTo GithubNoFormat.Object.ReleaseAsset
    -> SelectionSet (Maybe (List (Maybe decodesTo))) GithubNoFormat.Object.ReleaseAssetConnection
nodes object____ =
    Object.selectionForCompositeField "nodes" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| Information to aid in pagination.
-}
pageInfo :
    SelectionSet decodesTo GithubNoFormat.Object.PageInfo
    -> SelectionSet decodesTo GithubNoFormat.Object.ReleaseAssetConnection
pageInfo object____ =
    Object.selectionForCompositeField "pageInfo" [] object____ Basics.identity


{-| Identifies the total count of items in the connection.
-}
totalCount : SelectionSet Int GithubNoFormat.Object.ReleaseAssetConnection
totalCount =
    Object.selectionForField "Int" "totalCount" [] Decode.int
