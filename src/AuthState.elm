module AuthState exposing (AuthState(..), init)

import FireUser exposing (FireUser)


type AuthState
    = Loading
    | Authenticated FireUser
    | NoUser


init =
    Loading
