-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.BaseRefForcePushedEvent exposing (..)

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


{-| Identifies the actor who performed the event.
-}
actor :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.BaseRefForcePushedEvent
actor object____ =
    Object.selectionForCompositeField "actor" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the after commit SHA for the 'base\_ref\_force\_pushed' event.
-}
afterCommit :
    SelectionSet decodesTo GithubNoFormat.Object.Commit
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.BaseRefForcePushedEvent
afterCommit object____ =
    Object.selectionForCompositeField "afterCommit" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the before commit SHA for the 'base\_ref\_force\_pushed' event.
-}
beforeCommit :
    SelectionSet decodesTo GithubNoFormat.Object.Commit
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.BaseRefForcePushedEvent
beforeCommit object____ =
    Object.selectionForCompositeField "beforeCommit" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.BaseRefForcePushedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.BaseRefForcePushedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| PullRequest referenced by event.
-}
pullRequest :
    SelectionSet decodesTo GithubNoFormat.Object.PullRequest
    -> SelectionSet decodesTo GithubNoFormat.Object.BaseRefForcePushedEvent
pullRequest object____ =
    Object.selectionForCompositeField "pullRequest" [] object____ Basics.identity


{-| Identifies the fully qualified ref name for the 'base\_ref\_force\_pushed' event.
-}
ref :
    SelectionSet decodesTo GithubNoFormat.Object.Ref
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.BaseRefForcePushedEvent
ref object____ =
    Object.selectionForCompositeField "ref" [] object____ (Basics.identity >> Decode.nullable)
