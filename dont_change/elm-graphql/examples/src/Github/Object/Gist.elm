-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Github.Object.Gist exposing (..)

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


type alias CommentsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of comments associated with the gist

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
comments :
    (CommentsOptionalArguments -> CommentsOptionalArguments)
    -> SelectionSet decodesTo Github.Object.GistCommentConnection
    -> SelectionSet decodesTo Github.Object.Gist
comments fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "comments" optionalArgs____ object____ Basics.identity


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Github.ScalarCodecs.DateTime Github.Object.Gist
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The gist description.
-}
description : SelectionSet (Maybe String) Github.Object.Gist
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


id : SelectionSet Github.ScalarCodecs.Id Github.Object.Gist
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether the gist is public or not.
-}
isPublic : SelectionSet Bool Github.Object.Gist
isPublic =
    Object.selectionForField "Bool" "isPublic" [] Decode.bool


{-| The gist name.
-}
name : SelectionSet String Github.Object.Gist
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| The gist owner.
-}
owner :
    SelectionSet decodesTo Github.Interface.RepositoryOwner
    -> SelectionSet (Maybe decodesTo) Github.Object.Gist
owner object____ =
    Object.selectionForCompositeField "owner" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies when the gist was last pushed to.
-}
pushedAt : SelectionSet (Maybe Github.ScalarCodecs.DateTime) Github.Object.Gist
pushedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "pushedAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


type alias StargazersOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , orderBy : OptionalArgument Github.InputObject.StarOrder
    }


{-| A list of users who have starred this starrable.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - orderBy - Order for connection

-}
stargazers :
    (StargazersOptionalArguments -> StargazersOptionalArguments)
    -> SelectionSet decodesTo Github.Object.StargazerConnection
    -> SelectionSet decodesTo Github.Object.Gist
stargazers fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, orderBy = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "orderBy" filledInOptionals____.orderBy Github.InputObject.encodeStarOrder ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "stargazers" optionalArgs____ object____ Basics.identity


{-| Identifies the date and time when the object was last updated.
@deprecated General type updated timestamps will eventually be replaced by other field specific timestamps. Removal on 2018-07-01 UTC.
-}
updatedAt : SelectionSet Github.ScalarCodecs.DateTime Github.Object.Gist
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Github.ScalarCodecs.codecs |> Github.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Returns a boolean indicating whether the viewing user has starred this starrable.
-}
viewerHasStarred : SelectionSet Bool Github.Object.Gist
viewerHasStarred =
    Object.selectionForField "Bool" "viewerHasStarred" [] Decode.bool
