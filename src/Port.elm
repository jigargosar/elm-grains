port module Port exposing
    ( cacheGrains
    , cacheLabelList
    , cacheUserLabelList
    , error
    )

import Json.Encode exposing (Value)


port cacheGrains : Value -> Cmd msg


port cacheLabelList : Value -> Cmd msg


port cacheUserLabelList : Value -> Cmd msg


port error : String -> Cmd msg
