port module Port exposing
    ( autoSize
    , cacheGrains
    , error
    , fire2Elm
    , keyDownOnBody
    , logJson
    , navigateBack
    , persistGrains
    , persistNewGrain
    , persistRemovedGrain
    , persistSavedGrainList
    , persistUpdatedGrain
    , pushUrl
    , replaceState
    , setGrainCache
    , signIn
    , signOut
    , urlChanged
    )

import Json.Encode exposing (Value)


port keyDownOnBody : (Value -> msg) -> Sub msg


port autoSize : String -> Cmd msg


port persistSavedGrainList : Value -> Cmd msg


port cacheGrains : Value -> Cmd msg


port setGrainCache : Value -> Cmd msg


port persistGrains : Value -> Cmd msg


port persistNewGrain : Value -> Cmd msg


port persistUpdatedGrain : Value -> Cmd msg


port persistRemovedGrain : Value -> Cmd msg


port navigateBack : () -> Cmd msg


port error : String -> Cmd msg


port logJson : Value -> Cmd msg


port pushUrl : String -> Cmd msg


port replaceState : Value -> Cmd msg


port urlChanged : (Value -> msg) -> Sub msg


port fire2Elm : (Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg
