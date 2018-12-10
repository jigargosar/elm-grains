module Internal exposing
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


type alias Internal msg model =
    { handler : msg -> HandlerConfig msg model -> HandlerConfig msg model
    , model : model
    , cmd : Cmd msg
    }


type HandlerConfig msg model
    = HandlerConfig (Internal msg model)


dispatch : msg -> Internal msg model -> Internal msg model
dispatch msg config =
    config.handler msg config


mapModel fn config =
    { config | model = fn config.model }


andDo cmd config =
    { config | cmd = Cmd.batch [ config.cmd, cmd ] }


andDoWith :
    (model -> a)
    -> (a -> Cmd msg)
    -> Internal msg model
    -> Internal msg model
andDoWith extract fn c =
    andDo (fn (extract <| c.model)) c


andDoWhen pred cmd =
    when (.model >> pred) (andDo cmd)


andThenDo fn config =
    andDo (fn config.model) config


andThen fn c =
    fn c.model c


init initialHandler initialModel =
    Internal
        { handler = initialHandler
        , model = initialModel
        , cmd = Cmd.none
        }


toElmReturn =
    unwrap >> (\c -> ( c.model, c.cmd ))


toElmUpdateFn initialHandler initialMsg initialModel =
    init initialHandler initialModel |> dispatch initialMsg |> toElmReturn
