-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module GithubNoFormat.Object.Team exposing (..)

import GithubNoFormat.Enum.SubscriptionState
import GithubNoFormat.Enum.TeamMemberRole
import GithubNoFormat.Enum.TeamMembershipType
import GithubNoFormat.Enum.TeamPrivacy
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


type alias AncestorsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of teams that are ancestors of this team.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
ancestors :
    (AncestorsOptionalArguments -> AncestorsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.TeamConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Team
ancestors fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "ancestors" optionalArgs____ object____ Basics.identity


type alias AvatarUrlOptionalArguments =
    { size : OptionalArgument Int }


{-| A URL pointing to the team's avatar.

  - size - The size in pixels of the resulting square image.

-}
avatarUrl :
    (AvatarUrlOptionalArguments -> AvatarUrlOptionalArguments)
    -> SelectionSet (Maybe GithubNoFormat.ScalarCodecs.Uri) GithubNoFormat.Object.Team
avatarUrl fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { size = Absent }

        optionalArgs____ =
            [ Argument.optional "size" filledInOptionals____.size Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "avatarUrl" optionalArgs____ (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


type alias ChildTeamsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , orderBy : OptionalArgument GithubNoFormat.InputObject.TeamOrder
    , userLogins : OptionalArgument (List String)
    , immediateOnly : OptionalArgument Bool
    }


{-| List of child teams belonging to this team

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - orderBy - Order for connection
  - userLogins - User logins to filter by
  - immediateOnly - Whether to list immediate child teams or all descendant child teams.

-}
childTeams :
    (ChildTeamsOptionalArguments -> ChildTeamsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.TeamConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Team
childTeams fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, orderBy = Absent, userLogins = Absent, immediateOnly = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeTeamOrder, Argument.optional "userLogins" filledInOptionals____.userLogins (Encode.string |> Encode.list), Argument.optional "immediateOnly" filledInOptionals____.immediateOnly Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "childTeams" optionalArgs____ object____ Basics.identity


{-| The slug corresponding to the organization and team.
-}
combinedSlug : SelectionSet String GithubNoFormat.Object.Team
combinedSlug =
    Object.selectionForField "String" "combinedSlug" [] Decode.string


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.Team
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The description of the team.
-}
description : SelectionSet (Maybe String) GithubNoFormat.Object.Team
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for editing this team
-}
editTeamResourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
editTeamResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "editTeamResourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for editing this team
-}
editTeamUrl : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
editTeamUrl =
    Object.selectionForField "ScalarCodecs.Uri" "editTeamUrl" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


id : SelectionSet GithubNoFormat.ScalarCodecs.Id GithubNoFormat.Object.Team
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias InvitationsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    }


{-| A list of pending invitations for users to this team

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.

-}
invitations :
    (InvitationsOptionalArguments -> InvitationsOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.OrganizationInvitationConnection
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Team
invitations fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "invitations" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


type alias MembersOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , query : OptionalArgument String
    , membership : OptionalArgument GithubNoFormat.Enum.TeamMembershipType.TeamMembershipType
    , role : OptionalArgument GithubNoFormat.Enum.TeamMemberRole.TeamMemberRole
    }


{-| A list of users who are members of this team.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - query - The search string to look for.
  - membership - Filter by membership type
  - role - Filter by team member role

-}
members :
    (MembersOptionalArguments -> MembersOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.TeamMemberConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Team
members fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, query = Absent, membership = Absent, role = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "query" filledInOptionals____.query Encode.string, Argument.optional "membership" filledInOptionals____.membership (Encode.enum GithubNoFormat.Enum.TeamMembershipType.toString), Argument.optional "role" filledInOptionals____.role (Encode.enum GithubNoFormat.Enum.TeamMemberRole.toString) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "members" optionalArgs____ object____ Basics.identity


{-| The HTTP path for the team' members
-}
membersResourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
membersResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "membersResourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for the team' members
-}
membersUrl : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
membersUrl =
    Object.selectionForField "ScalarCodecs.Uri" "membersUrl" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The name of the team.
-}
name : SelectionSet String GithubNoFormat.Object.Team
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| The HTTP path creating a new team
-}
newTeamResourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
newTeamResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "newTeamResourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL creating a new team
-}
newTeamUrl : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
newTeamUrl =
    Object.selectionForField "ScalarCodecs.Uri" "newTeamUrl" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The organization that owns this team.
-}
organization :
    SelectionSet decodesTo GithubNoFormat.Object.Organization
    -> SelectionSet decodesTo GithubNoFormat.Object.Team
organization object____ =
    Object.selectionForCompositeField "organization" [] object____ Basics.identity


{-| The parent team of the team.
-}
parentTeam :
    SelectionSet decodesTo GithubNoFormat.Object.Team
    -> SelectionSet (Maybe decodesTo) GithubNoFormat.Object.Team
parentTeam object____ =
    Object.selectionForCompositeField "parentTeam" [] object____ (Basics.identity >> Decode.nullable)


{-| The level of privacy the team has.
-}
privacy : SelectionSet GithubNoFormat.Enum.TeamPrivacy.TeamPrivacy GithubNoFormat.Object.Team
privacy =
    Object.selectionForField "Enum.TeamPrivacy.TeamPrivacy" "privacy" [] GithubNoFormat.Enum.TeamPrivacy.decoder


type alias RepositoriesOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , query : OptionalArgument String
    , orderBy : OptionalArgument GithubNoFormat.InputObject.TeamRepositoryOrder
    }


{-| A list of repositories this team has access to.

  - first - Returns the first _n_ elements from the list.
  - after - Returns the elements in the list that come after the specified global ID.
  - last - Returns the last _n_ elements from the list.
  - before - Returns the elements in the list that come before the specified global ID.
  - query - The search string to look for.
  - orderBy - Order for the connection.

-}
repositories :
    (RepositoriesOptionalArguments -> RepositoriesOptionalArguments)
    -> SelectionSet decodesTo GithubNoFormat.Object.TeamRepositoryConnection
    -> SelectionSet decodesTo GithubNoFormat.Object.Team
repositories fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, query = Absent, orderBy = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "query" filledInOptionals____.query Encode.string, Argument.optional "orderBy" filledInOptionals____.orderBy GithubNoFormat.InputObject.encodeTeamRepositoryOrder ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "repositories" optionalArgs____ object____ Basics.identity


{-| The HTTP path for this team's repositories
-}
repositoriesResourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
repositoriesResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "repositoriesResourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this team's repositories
-}
repositoriesUrl : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
repositoriesUrl =
    Object.selectionForField "ScalarCodecs.Uri" "repositoriesUrl" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP path for this team
-}
resourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The slug corresponding to the team.
-}
slug : SelectionSet String GithubNoFormat.Object.Team
slug =
    Object.selectionForField "String" "slug" [] Decode.string


{-| The HTTP path for this team's teams
-}
teamsResourcePath : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
teamsResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "teamsResourcePath" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this team's teams
-}
teamsUrl : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
teamsUrl =
    Object.selectionForField "ScalarCodecs.Uri" "teamsUrl" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the date and time when the object was last updated.
@deprecated General type updated timestamps will eventually be replaced by other field specific timestamps. Removal on 2018-07-01 UTC.
-}
updatedAt : SelectionSet GithubNoFormat.ScalarCodecs.DateTime GithubNoFormat.Object.Team
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this team
-}
url : SelectionSet GithubNoFormat.ScalarCodecs.Uri GithubNoFormat.Object.Team
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (GithubNoFormat.ScalarCodecs.codecs |> GithubNoFormat.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Team is adminable by the viewer.
-}
viewerCanAdminister : SelectionSet Bool GithubNoFormat.Object.Team
viewerCanAdminister =
    Object.selectionForField "Bool" "viewerCanAdminister" [] Decode.bool


{-| Check if the viewer is able to change their subscription status for the repository.
-}
viewerCanSubscribe : SelectionSet Bool GithubNoFormat.Object.Team
viewerCanSubscribe =
    Object.selectionForField "Bool" "viewerCanSubscribe" [] Decode.bool


{-| Identifies if the viewer is watching, not watching, or ignoring the subscribable entity.
-}
viewerSubscription : SelectionSet GithubNoFormat.Enum.SubscriptionState.SubscriptionState GithubNoFormat.Object.Team
viewerSubscription =
    Object.selectionForField "Enum.SubscriptionState.SubscriptionState" "viewerSubscription" [] GithubNoFormat.Enum.SubscriptionState.decoder
