-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.SearchResultItemConnection exposing (..)

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


{-| The number of pieces of code that matched the search query.
-}
codeCount : SelectionSet Int Github.Object.SearchResultItemConnection
codeCount =
    Object.selectionForField "Int" "codeCount" [] Decode.int


{-| A list of edges.
-}
edges :
    SelectionSet decodesTo Github.Object.SearchResultItemEdge
    -> SelectionSet (Maybe (List (Maybe decodesTo))) Github.Object.SearchResultItemConnection
edges object____ =
    Object.selectionForCompositeField "edges" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| The number of issues that matched the search query.
-}
issueCount : SelectionSet Int Github.Object.SearchResultItemConnection
issueCount =
    Object.selectionForField "Int" "issueCount" [] Decode.int


{-| A list of nodes.
-}
nodes :
    SelectionSet decodesTo Github.Union.SearchResultItem
    -> SelectionSet (Maybe (List (Maybe decodesTo))) Github.Object.SearchResultItemConnection
nodes object____ =
    Object.selectionForCompositeField "nodes" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| Information to aid in pagination.
-}
pageInfo :
    SelectionSet decodesTo Github.Object.PageInfo
    -> SelectionSet decodesTo Github.Object.SearchResultItemConnection
pageInfo object____ =
    Object.selectionForCompositeField "pageInfo" [] object____ Basics.identity


{-| The number of repositories that matched the search query.
-}
repositoryCount : SelectionSet Int Github.Object.SearchResultItemConnection
repositoryCount =
    Object.selectionForField "Int" "repositoryCount" [] Decode.int


{-| The number of users that matched the search query.
-}
userCount : SelectionSet Int Github.Object.SearchResultItemConnection
userCount =
    Object.selectionForField "Int" "userCount" [] Decode.int


{-| The number of wiki pages that matched the search query.
-}
wikiCount : SelectionSet Int Github.Object.SearchResultItemConnection
wikiCount =
    Object.selectionForField "Int" "wikiCount" [] Decode.int
