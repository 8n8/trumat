-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.DeploymentStatus exposing (..)

import GithubNoFormat.Enum.DeploymentStatusState
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


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.DeploymentStatus
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Identifies the actor who triggered the deployment.
-}
creator :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.DeploymentStatus
creator object____ =
    Object.selectionForCompositeField "creator" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the deployment associated with status.
-}
deployment :
    SelectionSet decodesTo GithubNoFormat.Object.Deployment
    -> SelectionSet decodesTo GithubNoFormat.Object.DeploymentStatus
deployment object____ =
    Object.selectionForCompositeField "deployment" [] object____ Basics.identity


{-| Identifies the description of the deployment.
-}
description : SelectionSet (Maybe String) GithubNoFormat.Object.DeploymentStatus
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


{-| Identifies the environment URL of the deployment.
-}
environmentUrl : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.Uri) GithubNoFormat.Object.DeploymentStatus
environmentUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "environmentUrl" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.DeploymentStatus
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Identifies the log URL of the deployment.
-}
logUrl : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.Uri) GithubNoFormat.Object.DeploymentStatus
logUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "logUrl" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| Identifies the current state of the deployment.
-}
state : SelectionSet GithubNoFormat.Enum.DeploymentStatusState.DeploymentStatusState GithubNoFormat.Object.DeploymentStatus
state =
    Object.selectionForField "Enum.DeploymentStatusState.DeploymentStatusState" "state" [] GithubNoFormat.Enum.DeploymentStatusState.decoder


{-| Identifies the date and time when the object was last updated.
@deprecated General type updated timestamps will eventually be replaced by other field specific timestamps. Removal on 2018-07-01 UTC.
-}
updatedAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.DeploymentStatus
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
