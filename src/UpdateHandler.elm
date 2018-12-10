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


model =
    unwrap >> .model


unwrap (HandlerConfig hc) =
    hc


map fn =
    unwrap >> fn >> HandlerConfig


dispatch : msg -> HandlerConfig msg model -> HandlerConfig msg model
dispatch msg config =
    handler config msg config


mapModel fn =
    map (\c -> { c | model = fn c.model })


andDo cmd =
    map (\c -> { c | cmd = Cmd.batch [ c.cmd, cmd ] })


andDoWith :
    (model -> a)
    -> (a -> Cmd msg)
    -> HandlerConfig msg model
    -> HandlerConfig msg model
andDoWith extract fn c =
    andDo (fn (extract <| model c)) c


andDoWhen pred cmd =
    when (model >> pred) (andDo cmd)


andThenDo fn cmd =
    andDo (fn (model cmd)) cmd


andThen fn c =
    fn (model c) c


init initialHandler initialModel =
    HandlerConfig
        { handler = initialHandler
        , model = initialModel
        , cmd = Cmd.none
        }


toElmReturn =
    unwrap >> (\c -> ( c.model, c.cmd ))


toElmUpdateFn initialHandler initialMsg initialModel =
    init initialHandler initialModel |> dispatch initialMsg |> toElmReturn
