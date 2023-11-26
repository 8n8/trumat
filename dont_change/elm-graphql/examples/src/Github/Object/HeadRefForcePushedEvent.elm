-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.HeadRefForcePushedEvent exposing (..)

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


{-| Identifies the actor who performed the event.
-}
actor :
    SelectionSet decodesTo Github.Interface.Actor
    -> SelectionSet (Maybe decodesTo) Github.Object.HeadRefForcePushedEvent
actor object____ =
    Object.selectionForCompositeField "actor" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the after commit SHA for the 'head\_ref\_force\_pushed' event.
-}
afterCommit :
    SelectionSet decodesTo Github.Object.Commit
    -> SelectionSet (Maybe decodesTo) Github.Object.HeadRefForcePushedEvent
afterCommit object____ =
    Object.selectionForCompositeField "afterCommit" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the before commit SHA for the 'head\_ref\_force\_pushed' event.
-}
beforeCommit :
    SelectionSet decodesTo Github.Object.Commit
    -> SelectionSet (Maybe decodesTo) Github.Object.HeadRefForcePushedEvent
beforeCommit object____ =
    Object.selectionForCompositeField "beforeCommit" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Github.ScalarCodecs.DateTime Github.Object.HeadRefForcePushedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet Github.ScalarCodecs.Id Github.Object.HeadRefForcePushedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| PullRequest referenced by event.
-}
pullRequest :
    SelectionSet decodesTo Github.Object.PullRequest
    -> SelectionSet decodesTo Github.Object.HeadRefForcePushedEvent
pullRequest object____ =
    Object.selectionForCompositeField "pullRequest" [] object____ Basics.identity


{-| Identifies the fully qualified ref name for the 'head\_ref\_force\_pushed' event.
-}
ref :
    SelectionSet decodesTo Github.Object.Ref
    -> SelectionSet (Maybe decodesTo) Github.Object.HeadRefForcePushedEvent
ref object____ =
    Object.selectionForCompositeField "ref" [] object____ (Basics.identity >> Decode.nullable)