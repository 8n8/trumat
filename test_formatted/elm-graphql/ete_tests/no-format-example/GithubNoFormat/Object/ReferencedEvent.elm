-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.ReferencedEvent exposing (..)

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
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.ReferencedEvent
actor object____ =
    Object.selectionForCompositeField "actor" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the commit associated with the 'referenced' event.
-}
commit :
    SelectionSet decodesTo GithubNoFormat.Object.Commit
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.ReferencedEvent
commit object____ =
    Object.selectionForCompositeField "commit" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the repository associated with the 'referenced' event.
-}
commitRepository :
    SelectionSet decodesTo GithubNoFormat.Object.Repository
    -> SelectionSet decodesTo GithubNoFormat.Object.ReferencedEvent
commitRepository object____ =
    Object.selectionForCompositeField "commitRepository" [] object____ Basics.identity


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.ReferencedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.ReferencedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Reference originated in a different repository.
@deprecated `isCrossReference` will be renamed. Use `ReferencedEvent.isCrossRepository` instead. Removal on 2018-07-01 UTC.
-}
isCrossReference : SelectionSet Bool GithubNoFormat.Object.ReferencedEvent
isCrossReference =
    Object.selectionForField "Bool" "isCrossReference" [] Decode.bool


{-| Reference originated in a different repository.
-}
isCrossRepository : SelectionSet Bool GithubNoFormat.Object.ReferencedEvent
isCrossRepository =
    Object.selectionForField "Bool" "isCrossRepository" [] Decode.bool


{-| Checks if the commit message itself references the subject. Can be false in the case of a commit comment reference.
-}
isDirectReference : SelectionSet Bool GithubNoFormat.Object.ReferencedEvent
isDirectReference =
    Object.selectionForField "Bool" "isDirectReference" [] Decode.bool


{-| Object referenced by event.
-}
subject :
    SelectionSet decodesTo GithubNoFormat.Union.ReferencedSubject
    -> SelectionSet decodesTo GithubNoFormat.Object.ReferencedEvent
subject object____ =
    Object.selectionForCompositeField "subject" [] object____ Basics.identity
