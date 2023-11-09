-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.ProtectedBranch exposing (..)

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


{-| The actor who created this protected branch.
-}
creator :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.ProtectedBranch
creator object____ =
    Object.selectionForCompositeField "creator" [] object____ (Basics.identity >> Decode.nullable)


{-| Will new commits pushed to this branch dismiss pull request review approvals.
-}
hasDismissableStaleReviews : SelectionSet Bool GithubNoFormat.Object.ProtectedBranch
hasDismissableStaleReviews =
    Object.selectionForField "Bool" "hasDismissableStaleReviews" [] Decode.bool


{-| Are reviews required to update this branch.
-}
hasRequiredReviews : SelectionSet Bool GithubNoFormat.Object.ProtectedBranch
hasRequiredReviews =
    Object.selectionForField "Bool" "hasRequiredReviews" [] Decode.bool


{-| Are status checks required to update this branch.
-}
hasRequiredStatusChecks : SelectionSet Bool GithubNoFormat.Object.ProtectedBranch
hasRequiredStatusChecks =
    Object.selectionForField "Bool" "hasRequiredStatusChecks" [] Decode.bool


{-| Is pushing to this branch restricted.
-}
hasRestrictedPushes : SelectionSet Bool GithubNoFormat.Object.ProtectedBranch
hasRestrictedPushes =
    Object.selectionForField "Bool" "hasRestrictedPushes" [] Decode.bool


{-| Is dismissal of pull request reviews restricted.
-}
hasRestrictedReviewDismissals : SelectionSet Bool GithubNoFormat.Object.ProtectedBranch
hasRestrictedReviewDismissals =
    Object.selectionForField "Bool" "hasRestrictedReviewDismissals" [] Decode.bool


{-| Are branches required to be up to date before merging.
-}
hasStrictRequiredStatusChecks : SelectionSet Bool GithubNoFormat.Object.ProtectedBranch
hasStrictRequiredStatusChecks =
    Object.selectionForField "Bool" "hasStrictRequiredStatusChecks" [] Decode.bool


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.ProtectedBranch
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Can admins overwrite branch protection.
-}
isAdminEnforced : SelectionSet Bool GithubNoFormat.Object.ProtectedBranch
isAdminEnforced =
    Object.selectionForField "Bool" "isAdminEnforced" [] Decode.bool


{-| Identifies the name of the protected branch.
-}
name : SelectionSet String GithubNoFormat.Object.ProtectedBranch
name =
    Object.selectionForField "String" "name" [] Decode.string


type alias PushAllowancesOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list push allowances for this protected branch.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
pushAllowances :
    (PushAllowancesOptionalArguments -> PushAllowancesOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.PushAllowanceConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.ProtectedBranch
pushAllowances fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "pushAllowances" optionalArgs____ object____ Basics.identity


{-| The repository associated with this protected branch.
-}
repository :
    SelectionSet decodesTo GithubNoFormat.Object.Repository
    -> SelectionSet decodesTo GithubNoFormat.Object.ProtectedBranch
repository object____ =
    Object.selectionForCompositeField "repository" [] object____ Basics.identity


{-| List of required status check contexts that must pass for commits to be accepted to this branch.
-}
requiredStatusCheckContexts : SelectionSet (Maybe (List (Maybe String))) GithubNoFormat.Object.ProtectedBranch
requiredStatusCheckContexts =
    Object.selectionForField "(Maybe (List (Maybe String)))" "requiredStatusCheckContexts" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


type alias ReviewDismissalAllowancesOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list review dismissal allowances for this protected branch.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
reviewDismissalAllowances :
    (ReviewDismissalAllowancesOptionalArguments -> ReviewDismissalAllowancesOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.ReviewDismissalAllowanceConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.ProtectedBranch
reviewDismissalAllowances fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "reviewDismissalAllowances" optionalArgs____ object____ Basics.identity
