-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.Issue exposing (..)

import GithubNoFormat.Enum.CommentAuthorAssociation
import GithubNoFormat.Enum.CommentCannotUpdateReason
import GithubNoFormat.Enum.IssueState
import GithubNoFormat.Enum.LockReason
import GithubNoFormat.Enum.ReactionContent
import GithubNoFormat.Enum.SubscriptionState
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


{-| Reason that the conversation was locked.
-}
activeLockReason : SelectionSet (Maybe GithubNoFormat.Enum.LockReason.LockReason) GithubNoFormat.Object.Issue
activeLockReason =
    Object.selectionForField "(Maybe Enum.LockReason.LockReason)" "activeLockReason" [] (GithubNoFormat.Enum.LockReason.decoder |> Decode.nullable)


type alias AssigneesOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of Users assigned to this object.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
assignees :
    (AssigneesOptionalArguments -> AssigneesOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.UserConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Issue
assignees fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "assignees" optionalArgs____ object____ Basics.identity


{-| The actor who authored the comment.
-}
author :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Issue
author object____ =
    Object.selectionForCompositeField "author" [] object____ (Basics.identity >> Decode.nullable)


{-| Author's association with the subject of the comment.
-}
authorAssociation : SelectionSet GithubNoFormat.Enum.CommentAuthorAssociation.CommentAuthorAssociation GithubNoFormat.Object.Issue
authorAssociation =
    Object.selectionForField "Enum.CommentAuthorAssociation.CommentAuthorAssociation" "authorAssociation" [] GithubNoFormat.Enum.CommentAuthorAssociation.decoder


{-| Identifies the body of the issue.
-}
body : SelectionSet String GithubNoFormat.Object.Issue
body =
    Object.selectionForField "String" "body" [] Decode.string


{-| Identifies the body of the issue rendered to HTML.
-}
bodyHTML : SelectionSet GithubNoFormat.ScalarCodecs.Html GithubNoFormat.Object.Issue
bodyHTML =
    Object.selectionForField "ScalarCodecs.Html" "bodyHTML" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| Identifies the body of the issue rendered to text.
-}
bodyText : SelectionSet String GithubNoFormat.Object.Issue
bodyText =
    Object.selectionForField "String" "bodyText" [] Decode.string


{-| `true` if the object is closed (definition of closed may depend on type)
-}
closed : SelectionSet Bool GithubNoFormat.Object.Issue
closed =
    Object.selectionForField "Bool" "closed" [] Decode.bool


{-| Identifies the date and time when the object was closed.
-}
closedAt : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.DateTime) GithubNoFormat.Object.Issue
closedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "closedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


type alias CommentsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of comments associated with the Issue.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
comments :
    (CommentsOptionalArguments -> CommentsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.IssueCommentConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Issue
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
createdAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.Issue
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Check if this comment was created via an email reply.
-}
createdViaEmail : SelectionSet Bool GithubNoFormat.Object.Issue
createdViaEmail =
    Object.selectionForField "Bool" "createdViaEmail" [] Decode.bool


{-| Identifies the primary key from the database.
@deprecated Exposed database IDs will eventually be removed in favor of global Relay IDs. Use `Node.id` instead. Removal on 2018-07-01 UTC.
-}
databaseId : SelectionSet (Maybe Int) GithubNoFormat.Object.Issue
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| The actor who edited the comment.
-}
editor :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Issue
editor object____ =
    Object.selectionForCompositeField "editor" [] object____ (Basics.identity >> Decode.nullable)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.Issue
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias LabelsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of labels associated with the object.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
labels :
    (LabelsOptionalArguments -> LabelsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.LabelConnection
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Issue
labels fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "labels" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


{-| The moment the editor made the last edit
-}
lastEditedAt : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.DateTime) GithubNoFormat.Object.Issue
lastEditedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "lastEditedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| `true` if the object is locked
-}
locked : SelectionSet Bool GithubNoFormat.Object.Issue
locked =
    Object.selectionForField "Bool" "locked" [] Decode.bool


{-| Identifies the milestone associated with the issue.
-}
milestone :
    SelectionSet decodesTo GithubNoFormat.Object.Milestone
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Issue
milestone object____ =
    Object.selectionForCompositeField "milestone" [] object____ (Basics.identity >> Decode.nullable)


{-| Identifies the issue number.
-}
number : SelectionSet Int GithubNoFormat.Object.Issue
number =
    Object.selectionForField "Int" "number" [] Decode.int


type alias ParticipantsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of Users that are participating in the Issue conversation.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
participants :
    (ParticipantsOptionalArguments -> ParticipantsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.UserConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Issue
participants fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "participants" optionalArgs____ object____ Basics.identity


type alias ProjectCardsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| List of project cards associated with this issue.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
projectCards :
    (ProjectCardsOptionalArguments -> ProjectCardsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.ProjectCardConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Issue
projectCards fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "projectCards" optionalArgs____ object____ Basics.identity


{-| Identifies when the comment was published at.
-}
publishedAt : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.DateTime) GithubNoFormat.Object.Issue
publishedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "publishedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| A list of reactions grouped by content left on the subject.
-}
reactionGroups :
    SelectionSet decodesTo GithubNoFormat.Object.ReactionGroup
    -> SelectionSet (Maybe (List decodesTo)) GithubNoFormat.Object.Issue
reactionGroups object____ =
    Object.selectionForCompositeField "reactionGroups" [] object____ (Basics.identity >> Decode.list >> Decode.nullable)


type alias ReactionsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , content : OptionalArgument GithubNoFormat.Enum.ReactionContent.ReactionContent
    , orderBy : OptionalArgument GithubNoFormat.InputObject.ReactionOrder
    }


{-| A list of Reactions left on the Issue.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - content - Allows filtering Reactions by emoji.
  - orderBy - Allows specifying the order in which reactions are returned.

-}
reactions :
    (ReactionsOptionalArguments -> ReactionsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.ReactionConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Issue
reactions fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, content = Absent, orderBy = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "content" filledInOptionals____.content (Encode.enum GithubNoFormat.Enum.ReactionContent.toString), Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeReactionOrder ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "reactions" optionalArgs____ object____ Basics.identity


{-| The repository associated with this node.
-}
repository :
    SelectionSet decodesTo GithubNoFormat.Object.Repository
    -> SelectionSet decodesTo GithubNoFormat.Object.Issue
repository object____ =
    Object.selectionForCompositeField "repository" [] object____ Basics.identity


{-| The HTTP path for this issue
-}
resourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Issue
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the state of the issue.
-}
state : SelectionSet GithubNoFormat.Enum.IssueState.IssueState GithubNoFormat.Object.Issue
state =
    Object.selectionForField "Enum.IssueState.IssueState" "state" [] GithubNoFormat.Enum.IssueState.decoder


type alias TimelineOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , since : OptionalArgument GithubNoFormat.ScalarCodecs.DateTime
    }


{-| A list of events, comments, commits, etc. associated with the issue.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - since - Allows filtering timeline events by a `since` timestamp.

-}
timeline :
    (TimelineOptionalArguments -> TimelineOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.IssueTimelineConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Issue
timeline fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, since = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "since" filledInOptionals____.since (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapEncoder .codecDateTime) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "timeline" optionalArgs____ object____ Basics.identity


{-| Identifies the issue title.
-}
title : SelectionSet String GithubNoFormat.Object.Issue
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| Identifies the date and time when the object was last updated.
@deprecated General type updated timestamps will eventually be replaced by other field specific timestamps. Removal on 2018-07-01 UTC.
-}
updatedAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.Issue
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this issue
-}
url : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Issue
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


type alias UserContentEditsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of edits to this content.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
userContentEdits :
    (UserContentEditsOptionalArguments -> UserContentEditsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.UserContentEditConnection
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Issue
userContentEdits fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "userContentEdits" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


{-| Can user react to this subject
-}
viewerCanReact : SelectionSet Bool GithubNoFormat.Object.Issue
viewerCanReact =
    Object.selectionForField "Bool" "viewerCanReact" [] Decode.bool


{-| Check if the viewer is able to change their subscription status for the repository.
-}
viewerCanSubscribe : SelectionSet Bool GithubNoFormat.Object.Issue
viewerCanSubscribe =
    Object.selectionForField "Bool" "viewerCanSubscribe" [] Decode.bool


{-| Check if the current viewer can update this object.
-}
viewerCanUpdate : SelectionSet Bool GithubNoFormat.Object.Issue
viewerCanUpdate =
    Object.selectionForField "Bool" "viewerCanUpdate" [] Decode.bool


{-| Reasons why the current viewer can not update this comment.
-}
viewerCannotUpdateReasons : SelectionSet (List GithubNoFormat.Enum.CommentCannotUpdateReason.CommentCannotUpdateReason) GithubNoFormat.Object.Issue
viewerCannotUpdateReasons =
    Object.selectionForField "(List Enum.CommentCannotUpdateReason.CommentCannotUpdateReason)" "viewerCannotUpdateReasons" [] (GithubNoFormat.Enum.CommentCannotUpdateReason.decoder |> Decode.list)


{-| Did the viewer author this comment.
-}
viewerDidAuthor : SelectionSet Bool GithubNoFormat.Object.Issue
viewerDidAuthor =
    Object.selectionForField "Bool" "viewerDidAuthor" [] Decode.bool


{-| Identifies if the viewer is watching, not watching, or ignoring the subscribable entity.
-}
viewerSubscription : SelectionSet GithubNoFormat.Enum.SubscriptionState.SubscriptionState GithubNoFormat.Object.Issue
viewerSubscription =
    Object.selectionForField "Enum.SubscriptionState.SubscriptionState" "viewerSubscription" [] GithubNoFormat.Enum.SubscriptionState.decoder
