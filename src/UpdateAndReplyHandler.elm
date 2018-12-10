module UpdateAndReplyHandler exposing (ReplyHandleConfig)

import BasicsX exposing (when)


type alias ReplyHandleConfig msg model reply =
    { msg : msg, model : model, replies : List reply }
