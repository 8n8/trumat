-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Union.SearchResultItem exposing (..)

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
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onIssue : SelectionSet decodesTo GithubNoFormat.Object.Issue
    , onPullRequest : SelectionSet decodesTo GithubNoFormat.Object.PullRequest
    , onRepository : SelectionSet decodesTo GithubNoFormat.Object.Repository
    , onUser : SelectionSet decodesTo GithubNoFormat.Object.User
    , onOrganization : SelectionSet decodesTo GithubNoFormat.Object.Organization
    , onMarketplaceListing : SelectionSet decodesTo GithubNoFormat.Object.MarketplaceListing
    }


{-| Build up a selection for this Union by passing in a Fragments record.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo GithubNoFormat.Union.SearchResultItem
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "Issue" selections____.onIssue
        , Object.buildFragment "PullRequest" selections____.onPullRequest
        , Object.buildFragment "Repository" selections____.onRepository
        , Object.buildFragment "User" selections____.onUser
        , Object.buildFragment "Organization" selections____.onOrganization
        , Object.buildFragment "MarketplaceListing" selections____.onMarketplaceListing
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onIssue = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onPullRequest = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepository = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onUser = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onOrganization = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onMarketplaceListing = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
