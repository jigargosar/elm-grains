module AuthState exposing (AuthState)

import FireUser exposing (FireUser)


type AuthState
    = Unknown
    | Authenticated FireUser
    | NoUser
