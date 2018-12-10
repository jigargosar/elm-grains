module UpdateHandler exposing
    ( HandlerConfig
    , andDo
    , andDoWhen
    , andDoWith
    , andThen
    , andThenDo
    , dispatch
    , mapModel
    , toElmUpdateFn
    )

import BasicsX exposing (when)


type HandlerConfig msg model
    = HandlerConfig
        { handler : msg -> HandlerConfig msg model -> HandlerConfig msg model
        , model : model
        , cmd : Cmd msg
        }


handler =
    unwrap >> .handler


modelFromConfig =
    unwrap >> .model


unwrap (HandlerConfig hc) =
    hc


mapConfig fn =
    unwrap >> fn >> HandlerConfig


dispatch : msg -> HandlerConfig msg model -> HandlerConfig msg model
dispatch msg config =
    handler config msg config


mapModel fn =
    mapConfig (\c -> { c | model = fn c.model })


andDo cmd =
    mapConfig (\c -> { c | cmd = Cmd.batch [ c.cmd, cmd ] })


andDoWith :
    (model -> a)
    -> (a -> Cmd msg)
    -> HandlerConfig msg model
    -> HandlerConfig msg model
andDoWith extract fn c =
    andDo (fn (extract <| modelFromConfig c)) c


andDoWhen pred cmd =
    when (modelFromConfig >> pred) (andDo cmd)


andThenDo fn cmd =
    andDo (fn (modelFromConfig cmd)) cmd


andThen fn c =
    fn (modelFromConfig c) c


init initialHandler initialModel =
    HandlerConfig
        { handler = initialHandler
        , model = initialModel
        , cmd = Cmd.none
        }


toElmUpdateFn handlerFn msg model =
    let
        config =
            init handlerFn model

        toReturn =
            unwrap >> (\c -> ( c.model, c.cmd ))
    in
    dispatch msg config |> toReturn
