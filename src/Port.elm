port module Port exposing
    ( cacheGrainList
    , cacheLabelList
    , cacheUserLabelList
    , error
    )

import Json.Encode exposing (Value)


port cacheGrainList : Value -> Cmd msg


port cacheLabelList : Value -> Cmd msg


port cacheUserLabelList : Value -> Cmd msg


port error : String -> Cmd msg
