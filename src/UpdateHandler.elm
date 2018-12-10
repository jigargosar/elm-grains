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


handlerFromConfig =
    unwrapConfig >> .handler


modelFromConfig =
    unwrapConfig >> .model


unwrapConfig (HandlerConfig hc) =
    hc


mapConfig fn =
    unwrapConfig >> fn >> HandlerConfig


dispatch : msg -> HandlerConfig msg model -> HandlerConfig msg model
dispatch msg config =
    handlerFromConfig config msg config


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


toElmUpdateFn handler msg model =
    let
        config =
            HandlerConfig
                { handler = handler
                , model = model
                , cmd = Cmd.none
                }

        toReturn =
            unwrapConfig >> (\c -> ( c.model, c.cmd ))
    in
    handlerFromConfig config msg config |> toReturn
