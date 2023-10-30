-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.DeployedEvent exposing (..)

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
    -> SelectionSet (Maybe decodesTo) Github.Object.DeployedEvent
actor object____ =
    Object.selectionForCompositeField "actor" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Github.ScalarCodecs.DateTime Github.Object.DeployedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Identifies the primary key from the database.
@deprecated Exposed database IDs will eventually be removed in favor of global Relay IDs. Use `Node.id` instead. Removal on 2018-07-01 UTC.
-}
databaseId : SelectionSet (Maybe Int) Github.Object.DeployedEvent
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| The deployment associated with the 'deployed' event.
-}
deployment :
    SelectionSet decodesTo Github.Object.Deployment
    -> SelectionSet decodesTo Github.Object.DeployedEvent
deployment object____ =
    Object.selectionForCompositeField "deployment" [] object____ Basics.identity


id : SelectionSet Github.ScalarCodecs.Id Github.Object.DeployedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| PullRequest referenced by event.
-}
pullRequest :
    SelectionSet decodesTo Github.Object.PullRequest
    -> SelectionSet decodesTo Github.Object.DeployedEvent
pullRequest object____ =
    Object.selectionForCompositeField "pullRequest" [] object____ Basics.identity


{-| The ref associated with the 'deployed' event.
-}
ref :
    SelectionSet decodesTo Github.Object.Ref
    -> SelectionSet (Maybe decodesTo) Github.Object.DeployedEvent
ref object____ =
    Object.selectionForCompositeField "ref" [] object____ (Basics.identity >> Decode.nullable)