-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.PullRequestReviewComment exposing (..)

import GithubNoFormat.Enum.CommentAuthorAssociation
import GithubNoFormat.Enum.CommentCannotUpdateReason
import GithubNoFormat.Enum.ReactionContent
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


{-| The actor who authored the comment.
-}
author :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.PullRequestReviewComment
author object____ =
    Object.selectionForCompositeField "author" [] object____ (Basics.identity >> Decode.nullable)


{-| Author's association with the subject of the comment.
-}
authorAssociation : SelectionSet GithubNoFormat.Enum.CommentAuthorAssociation.CommentAuthorAssociation GithubNoFormat.Object.PullRequestReviewComment
authorAssociation =
    Object.selectionForField "Enum.CommentAuthorAssociation.CommentAuthorAssociation" "authorAssociation" [] GithubNoFormat.Enum.CommentAuthorAssociation.decoder


{-| The comment body of this review comment.
-}
body : SelectionSet String GithubNoFormat.Object.PullRequestReviewComment
body =
    Object.selectionForField "String" "body" [] Decode.string


{-| The comment body of this review comment rendered to HTML.
-}
bodyHTML : SelectionSet GithubNoFormat.ScalarCodecs.Html GithubNoFormat.Object.PullRequestReviewComment
bodyHTML =
    Object.selectionForField "ScalarCodecs.Html" "bodyHTML" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| The comment body of this review comment rendered as plain text.
-}
bodyText : SelectionSet String GithubNoFormat.Object.PullRequestReviewComment
bodyText =
    Object.selectionForField "String" "bodyText" [] Decode.string


{-| Identifies the commit associated with the comment.
-}
commit :
    SelectionSet decodesTo GithubNoFormat.Object.Commit
    -> SelectionSet decodesTo GithubNoFormat.Object.PullRequestReviewComment
commit object____ =
    Object.selectionForCompositeField "commit" [] object____ Basics.identity


{-| Identifies when the comment was created.
-}
createdAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.PullRequestReviewComment
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Check if this comment was created via an email reply.
-}
createdViaEmail : SelectionSet Bool GithubNoFormat.Object.PullRequestReviewComment
createdViaEmail =
    Object.selectionForField "Bool" "createdViaEmail" [] Decode.bool


{-| Identifies the primary key from the database.
@deprecated Exposed database IDs will eventually be removed in favor of global Relay IDs. Use `Node.id` instead. Removal on 2018-07-01 UTC.
-}
databaseId : SelectionSet (Maybe Int) GithubNoFormat.Object.PullRequestReviewComment
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| The diff hunk to which the comment applies.
-}
diffHunk : SelectionSet String GithubNoFormat.Object.PullRequestReviewComment
diffHunk =
    Object.selectionForField "String" "diffHunk" [] Decode.string


{-| Identifies when the comment was created in a draft state.
-}
draftedAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.PullRequestReviewComment
draftedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "draftedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The actor who edited the comment.
-}
editor :
    SelectionSet decodesTo GithubNoFormat.Interface.Actor
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.PullRequestReviewComment
editor object____ =
    Object.selectionForCompositeField "editor" [] object____ (Basics.identity >> Decode.nullable)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.PullRequestReviewComment
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The moment the editor made the last edit
-}
lastEditedAt : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.DateTime) GithubNoFormat.Object.PullRequestReviewComment
lastEditedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "lastEditedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| Identifies the original commit associated with the comment.
-}
originalCommit :
    SelectionSet decodesTo GithubNoFormat.Object.Commit
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.PullRequestReviewComment
originalCommit object____ =
    Object.selectionForCompositeField "originalCommit" [] object____ (Basics.identity >> Decode.nullable)


{-| The original line index in the diff to which the comment applies.
-}
originalPosition : SelectionSet Int GithubNoFormat.Object.PullRequestReviewComment
originalPosition =
    Object.selectionForField "Int" "originalPosition" [] Decode.int


{-| The path to which the comment applies.
-}
path : SelectionSet String GithubNoFormat.Object.PullRequestReviewComment
path =
    Object.selectionForField "String" "path" [] Decode.string


{-| The line index in the diff to which the comment applies.
-}
position : SelectionSet (Maybe Int) GithubNoFormat.Object.PullRequestReviewComment
position =
    Object.selectionForField "(Maybe Int)" "position" [] (Decode.int |> Decode.nullable)


{-| Identifies when the comment was published at.
-}
publishedAt : SelectionSet (Maybe GithubNoFormat.ScalarCodecs.DateTime) GithubNoFormat.Object.PullRequestReviewComment
publishedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "publishedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| The pull request associated with this review comment.
-}
pullRequest :
    SelectionSet decodesTo GithubNoFormat.Object.PullRequest
    -> SelectionSet decodesTo GithubNoFormat.Object.PullRequestReviewComment
pullRequest object____ =
    Object.selectionForCompositeField "pullRequest" [] object____ Basics.identity


{-| The pull request review associated with this review comment.
-}
pullRequestReview :
    SelectionSet decodesTo GithubNoFormat.Object.PullRequestReview
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.PullRequestReviewComment
pullRequestReview object____ =
    Object.selectionForCompositeField "pullRequestReview" [] object____ (Basics.identity >> Decode.nullable)


{-| A list of reactions grouped by content left on the subject.
-}
reactionGroups :
    SelectionSet decodesTo GithubNoFormat.Object.ReactionGroup
    -> SelectionSet (Maybe (List decodesTo)) GithubNoFormat.Object.PullRequestReviewComment
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
    -> SelectionSet decodesTo GithubNoFormat.Object.PullRequestReviewComment
reactions fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, content = Absent, orderBy = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "content" filledInOptionals____.content (Encode.enum GithubNoFormat.Enum.ReactionContent.toString), Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeReactionOrder ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "reactions" optionalArgs____ object____ Basics.identity


{-| The comment this is a reply to.
-}
replyTo :
    SelectionSet decodesTo GithubNoFormat.Object.PullRequestReviewComment
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.PullRequestReviewComment
replyTo object____ =
    Object.selectionForCompositeField "replyTo" [] object____ (Basics.identity >> Decode.nullable)


{-| The repository associated with this node.
-}
repository :
    SelectionSet decodesTo GithubNoFormat.Object.Repository
    -> SelectionSet decodesTo GithubNoFormat.Object.PullRequestReviewComment
repository object____ =
    Object.selectionForCompositeField "repository" [] object____ Basics.identity


{-| The HTTP path permalink for this review comment.
-}
resourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.PullRequestReviewComment
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies when the comment was last updated.
-}
updatedAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.PullRequestReviewComment
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL permalink for this review comment.
-}
url : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.PullRequestReviewComment
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
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.PullRequestReviewComment
userContentEdits fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "userContentEdits" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


{-| Check if the current viewer can delete this object.
-}
viewerCanDelete : SelectionSet Bool GithubNoFormat.Object.PullRequestReviewComment
viewerCanDelete =
    Object.selectionForField "Bool" "viewerCanDelete" [] Decode.bool


{-| Can user react to this subject
-}
viewerCanReact : SelectionSet Bool GithubNoFormat.Object.PullRequestReviewComment
viewerCanReact =
    Object.selectionForField "Bool" "viewerCanReact" [] Decode.bool


{-| Check if the current viewer can update this object.
-}
viewerCanUpdate : SelectionSet Bool GithubNoFormat.Object.PullRequestReviewComment
viewerCanUpdate =
    Object.selectionForField "Bool" "viewerCanUpdate" [] Decode.bool


{-| Reasons why the current viewer can not update this comment.
-}
viewerCannotUpdateReasons : SelectionSet (List GithubNoFormat.Enum.CommentCannotUpdateReason.CommentCannotUpdateReason) GithubNoFormat.Object.PullRequestReviewComment
viewerCannotUpdateReasons =
    Object.selectionForField "(List Enum.CommentCannotUpdateReason.CommentCannotUpdateReason)" "viewerCannotUpdateReasons" [] (GithubNoFormat.Enum.CommentCannotUpdateReason.decoder |> Decode.list)


{-| Did the viewer author this comment.
-}
viewerDidAuthor : SelectionSet Bool GithubNoFormat.Object.PullRequestReviewComment
viewerDidAuthor =
    Object.selectionForField "Bool" "viewerDidAuthor" [] Decode.bool
