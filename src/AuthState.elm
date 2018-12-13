module AuthState exposing (AuthState(..), init)

import FireUser exposing (FireUser)


type AuthState
    = Unknown
    | Authenticated FireUser
    | NoUser


init =
    Unknown
