port module Port exposing
    ( cacheGrains
    , error
    , fire2Elm
    , persistGrains
    , persistNewGrain
    , persistUpdatedGrain
    , pushUrl
    , signIn
    , signOut
    , urlChanged
    )

import Json.Encode exposing (Value)


port cacheGrains : Value -> Cmd msg


port persistGrains : Value -> Cmd msg


port persistNewGrain : Value -> Cmd msg


port persistUpdatedGrain : Value -> Cmd msg


port error : String -> Cmd msg


port pushUrl : String -> Cmd msg


port urlChanged : (String -> msg) -> Sub msg


port fire2Elm : (Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg
