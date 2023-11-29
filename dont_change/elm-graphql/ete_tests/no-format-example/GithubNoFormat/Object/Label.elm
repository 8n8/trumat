-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.Label exposing (..)

import GithubNoFormat.Enum.IssueState
import GithubNoFormat.Enum.PullRequestState
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


{-| Identifies the label color.
-}
color : SelectionSet String GithubNoFormat.Object.Label
color =
    Object.selectionForField "String" "color" [] Decode.string


{-| A brief description of this label.
-}
description : SelectionSet (Maybe String) GithubNoFormat.Object.Label
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.Label
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Indicates whether or not this is a default label.
-}
isDefault : SelectionSet Bool GithubNoFormat.Object.Label
isDefault =
    Object.selectionForField "Bool" "isDefault" [] Decode.bool


type alias IssuesOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , labels : OptionalArgument (List String)
    , orderBy : OptionalArgument GithubNoFormat.InputObject.IssueOrder
    , states : OptionalArgument (List GithubNoFormat.Enum.IssueState.IssueState)
    }


{-| A list of issues associated with this label.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - labels - A list of label names to filter the pull requests by.
  - orderBy - Ordering options for issues returned from the connection.
  - states - A list of states to filter the issues by.

-}
issues :
    (IssuesOptionalArguments -> IssuesOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.IssueConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Label
issues fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, labels = Absent, orderBy = Absent, states = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "labels" filledInOptionals____.labels (Encode.string |> Encode.list), Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeIssueOrder, Argument.optional "states" filledInOptionals____.states (Encode.enum GithubNoFormat.Enum.IssueState.toString |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "issues" optionalArgs____ object____ Basics.identity


{-| Identifies the label name.
-}
name : SelectionSet String GithubNoFormat.Object.Label
name =
    Object.selectionForField "String" "name" [] Decode.string


type alias PullRequestsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , states : OptionalArgument (List GithubNoFormat.Enum.PullRequestState.PullRequestState)
    , labels : OptionalArgument (List String)
    , headRefName : OptionalArgument String
    , baseRefName : OptionalArgument String
    , orderBy : OptionalArgument GithubNoFormat.InputObject.IssueOrder
    }


{-| A list of pull requests associated with this label.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - states - A list of states to filter the pull requests by.
  - labels - A list of label names to filter the pull requests by.
  - headRefName - The head ref name to filter the pull requests by.
  - baseRefName - The base ref name to filter the pull requests by.
  - orderBy - Ordering options for pull requests returned from the connection.

-}
pullRequests :
    (PullRequestsOptionalArguments -> PullRequestsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.PullRequestConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Label
pullRequests fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, states = Absent, labels = Absent, headRefName = Absent, baseRefName = Absent, orderBy = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "states" filledInOptionals____.states (Encode.enum GithubNoFormat.Enum.PullRequestState.toString |> Encode.list), Argument.optional "labels" filledInOptionals____.labels (Encode.string |> Encode.list), Argument.optional "headRefName" filledInOptionals____.headRefName Encode.string, Argument.optional "baseRefName" filledInOptionals____.baseRefName Encode.string, Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeIssueOrder ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "pullRequests" optionalArgs____ object____ Basics.identity


{-| The repository associated with this label.
-}
repository :
    SelectionSet decodesTo GithubNoFormat.Object.Repository
    -> SelectionSet decodesTo GithubNoFormat.Object.Label
repository object____ =
    Object.selectionForCompositeField "repository" [] object____ Basics.identity
