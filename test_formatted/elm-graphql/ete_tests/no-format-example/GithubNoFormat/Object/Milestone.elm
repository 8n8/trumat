-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.Milestone exposing (..)

import GithubNoFormat.Enum.IssueState
import GithubNoFormat.Enum.MilestoneState
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


{-| `true` if the object is closed (definition of closed may depend on type)
-}
closed : SelectionSet Bool GithubNoFormat.Object.Milestone
closed =
    Object.selectionForField "Bool" "closed" [] Decode.bool


{-| Identifies the date and time when the object was closed.
-}
closedAt : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.DateTime) GithubNoFormat.Object.Milestone
closedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "closedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.Milestone
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Identifies the actor who created the milestone.
-}
creator :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Milestone
creator object____ =
    Object.selectionForCompositeField "creator" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the description of the milestone.
-}
description : SelectionSet (Maybe String) GithubNoFormat.Object.Milestone
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


{-| Identifies the due date of the milestone.
-}
dueOn : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.DateTime) GithubNoFormat.Object.Milestone
dueOn =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "dueOn" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.Milestone
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias IssuesOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , labels : OptionalArgument (List String)
    , orderBy : OptionalArgument GithubNoFormat.InputObject.IssueOrder
    , states : OptionalArgument (List GithubNoFormat.Enum.IssueState.IssueState)
    }


{-| A list of issues associated with the milestone.

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
    -> SelectionSet decodesTo GithubNoFormat.Object.Milestone
issues fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, labels = Absent, orderBy = Absent, states = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "labels" filledInOptionals____.labels (Encode.string |> Encode.list), Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeIssueOrder, Argument.optional "states" filledInOptionals____.states (Encode.enum GithubNoFormat.Enum.IssueState.toString |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "issues" optionalArgs____ object____ Basics.identity


{-| Identifies the number of the milestone.
-}
number : SelectionSet Int GithubNoFormat.Object.Milestone
number =
    Object.selectionForField "Int" "number" [] Decode.int


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


{-| A list of pull requests associated with the milestone.

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
    -> SelectionSet decodesTo GithubNoFormat.Object.Milestone
pullRequests fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, states = Absent, labels = Absent, headRefName = Absent, baseRefName = Absent, orderBy = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "states" filledInOptionals____.states (Encode.enum GithubNoFormat.Enum.PullRequestState.toString |> Encode.list), Argument.optional "labels" filledInOptionals____.labels (Encode.string |> Encode.list), Argument.optional "headRefName" filledInOptionals____.headRefName Encode.string, Argument.optional "baseRefName" filledInOptionals____.baseRefName Encode.string, Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeIssueOrder ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "pullRequests" optionalArgs____ object____ Basics.identity


{-| The repository associated with this milestone.
-}
repository :
    SelectionSet decodesTo GithubNoFormat.Object.Repository
    -> SelectionSet decodesTo GithubNoFormat.Object.Milestone
repository object____ =
    Object.selectionForCompositeField "repository" [] object____ Basics.identity


{-| The HTTP path for this milestone
-}
resourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Milestone
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the state of the milestone.
-}
state : SelectionSet GithubNoFormat.Enum.MilestoneState.MilestoneState GithubNoFormat.Object.Milestone
state =
    Object.selectionForField "Enum.MilestoneState.MilestoneState" "state" [] GithubNoFormat.Enum.MilestoneState.decoder


{-| Identifies the title of the milestone.
-}
title : SelectionSet String GithubNoFormat.Object.Milestone
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| Identifies the date and time when the object was last updated.
@deprecated General type updated timestamps will eventually be replaced by other field specific timestamps. Removal on 2018-07-01 UTC.
-}
updatedAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.Milestone
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this milestone
-}
url : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Milestone
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)
