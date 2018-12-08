module UpdateHandler exposing
    ( andDo
    , andDoWhen
    , andThenDo
    , mapModel
    , toElmUpdateFn
    )

import BasicsX exposing (when)


handlerFromConfig =
    unwrapConfig >> .handler


modelFromConfig =
    unwrapConfig >> .model


unwrapConfig (HandlerConfig hc) =
    hc


overConfig fn =
    unwrapConfig >> fn >> HandlerConfig


dispatch : msg -> HandlerConfig msg model -> HandlerConfig msg model
dispatch msg config =
    handlerFromConfig config msg config


mapModel fn =
    overConfig (\c -> { c | model = fn c.model })


andDo cmd =
    overConfig (\c -> { c | cmd = Cmd.batch [ c.cmd, cmd ] })


andDoWhen pred cmd =
    when (modelFromConfig >> pred) (andDo cmd)


andThenDo fn cmd =
    andDo (fn (modelFromConfig cmd)) cmd


type HandlerConfig msg model
    = HandlerConfig
        { handler : msg -> HandlerConfig msg model -> HandlerConfig msg model
        , model : model
        , cmd : Cmd msg
        }


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
