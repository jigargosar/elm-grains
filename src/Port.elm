port module Port exposing
    ( cacheGrains
    , error
    , pushUrl
    , urlChanged
    )

import Json.Encode exposing (Value)


port cacheGrains : Value -> Cmd msg


port error : String -> Cmd msg


port pushUrl : String -> Cmd msg


port urlChanged : (String -> msg) -> Sub msg
