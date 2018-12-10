module UpdateAndReplyHandler exposing (ReplyHandleConfig, mapModel)

import BasicsX exposing (when)


type alias ReplyHandleConfig msg model reply =
    { msg : msg, model : model, replies : List reply }


mapModel fn config =
    { config | model = fn config.model }
