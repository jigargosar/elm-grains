port module Port exposing
    ( cacheGrains
    , error
    , fire2Elm
    , navigateBack
    , persistGrains
    , persistNewGrain
    , persistRemovedGrain
    , persistSavedGrainList
    , persistUpdatedGrain
    , pushUrl
    , setGrainCache
    , signIn
    , signOut
    , urlChanged
    )

import Json.Encode exposing (Value)


port persistSavedGrainList : Value -> Cmd msg


port cacheGrains : Value -> Cmd msg


port setGrainCache : Value -> Cmd msg


port persistGrains : Value -> Cmd msg


port persistNewGrain : Value -> Cmd msg


port persistUpdatedGrain : Value -> Cmd msg


port persistRemovedGrain : Value -> Cmd msg


port navigateBack : () -> Cmd msg


port error : String -> Cmd msg


port pushUrl : String -> Cmd msg


port urlChanged : (String -> msg) -> Sub msg


port fire2Elm : (Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg
