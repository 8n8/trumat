module Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)

{-| This module contains types used to annotate top-level queries which can be
built up using functions in code generated by the `@dillonkearns/elm-graphql` command line tool
and sent using functions in the `Graphql.Http` module.

@docs RootMutation, RootQuery, RootSubscription

-}


{-| Type for top-level queries which can be sent using functions
from `Graphql.Http`.
-}
type RootQuery
    = RootQuery


{-| Type for top-level mutations which can be sent using functions
from `Graphql.Http`.
-}
type RootMutation
    = RootMutation


{-| Type for top-level subscriptions. Subscriptions use WebSockets, for which Elm needs ports. See
<https://github.com/dillonkearns/elm-graphql/blob/master/examples/subscription/Main.elm> for an example of using
`elm-graphql` subscriptions with ports.
-}
type RootSubscription
    = RootSubscription
