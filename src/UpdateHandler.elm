module UpdateHandler exposing
    ( HandlerConfig
    , andDo
    , andDoWhen
    , andDoWith
    , andThen
    , andThenDo
    , dispatch
    , dispatchSub
    , mapModel
    , toElmUpdateFn
    )

import BasicsX exposing (when)


type HandlerConfig msg model reply
    = HandlerConfig
        { handler : msg -> HandlerConfig msg model reply -> HandlerConfig msg model reply
        , model : model
        , cmd : Cmd msg
        , reply : List reply
        }


handlerFromConfig =
    unwrapConfig >> .handler


modelFromConfig =
    unwrapConfig >> .model


unwrapConfig (HandlerConfig hc) =
    hc


mapConfig fn =
    unwrapConfig >> fn >> HandlerConfig


dispatch : msg -> HandlerConfig msg model reply -> HandlerConfig msg model reply
dispatch msg config =
    handlerFromConfig config msg config


dispatchSub msg { handler, toMsg, get, set, toReply } parentConfig =
    let
        childReturnConfig =
            HandlerConfig
                { handler = handler
                , model = get (modelFromConfig parentConfig)
                , cmd = Cmd.none
                , reply = []
                }
                |> dispatch msg
    in
    childReturnConfig
        |> mapConfig
            (\cc ->
                let
                    parentConfigInternal =
                        unwrapConfig parentConfig
                in
                { parentConfigInternal
                    | model = set cc.model parentConfigInternal.model
                    , cmd = Cmd.map toMsg cc.cmd
                }
            )


mapModel fn =
    mapConfig (\c -> { c | model = fn c.model })


andDo cmd =
    mapConfig (\c -> { c | cmd = Cmd.batch [ c.cmd, cmd ] })


andDoWith :
    (model -> a)
    -> (a -> Cmd msg)
    -> HandlerConfig msg model reply
    -> HandlerConfig msg model reply
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
                , reply = []
                }

        toReturn =
            unwrapConfig >> (\c -> ( c.model, c.cmd ))
    in
    handlerFromConfig config msg config |> toReturn
