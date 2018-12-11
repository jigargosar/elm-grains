port module Port exposing
    ( cacheGrains
    , cacheLabelList
    , cacheUserLabelList
    , error
    , pushUrl
    , urlChanged
    )

import Json.Encode exposing (Value)


port cacheGrains : Value -> Cmd msg


port cacheLabelList : Value -> Cmd msg


port cacheUserLabelList : Value -> Cmd msg


port error : String -> Cmd msg


port pushUrl : String -> Cmd msg


port urlChanged : (String -> msg) -> Sub msg
