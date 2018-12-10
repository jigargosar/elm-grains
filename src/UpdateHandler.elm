module UpdateHandler exposing
    ( HandlerConfig
    , andDo
    , andDoWhen
    , andDoWith
    , andThen
    , andThenDo
    , mapModel
    , toElmUpdateFn
    )

import BasicsX exposing (when)
import UpdateHandler.Internal as Internal exposing (Internal)


type HandlerConfig msg model
    = Private (Internal msg model)


unwrap : HandlerConfig msg model -> Internal msg model
unwrap (Private internal) =
    internal


map :
    (Internal msg model -> Internal msg model)
    -> HandlerConfig msg model
    -> HandlerConfig msg model
map fn =
    unwrap >> fn >> Private


mapModel fn =
    map (\c -> { c | model = fn c.model })


andDo cmd =
    map <| Internal.andDo cmd


andDoWith :
    (model -> a)
    -> (a -> Cmd msg)
    -> HandlerConfig msg model
    -> HandlerConfig msg model
andDoWith extract fn =
    map <| Internal.andDoWith extract fn


andDoWhen pred cmd =
    map <| Internal.andDoWhen pred cmd


andThenDo fn =
    map <| Internal.andThenDo fn


andThen fn =
    map <| Internal.andThen fn


lift fn =
    Private >> fn >> unwrap


lift2 fn a =
    Private >> fn a >> unwrap


toElmUpdateFn handler =
    Internal.toElmUpdateFn (lift2 handler)
