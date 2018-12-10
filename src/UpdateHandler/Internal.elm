module UpdateHandler.Internal exposing
    ( Internal
    , andDo
    , andDoWhen
    , andDoWith
    , andThen
    , andThenDo
    , init
    , mapModel
    , toElmUpdateFn
    )

import BasicsX exposing (when)


type alias Internal msg model =
    { model : model
    , cmd : Cmd msg
    }


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


toElmReturn config =
    ( config.model, config.cmd )


init model =
    { model = model
    , cmd = Cmd.none
    }


type alias Handler msg model =
    msg -> Internal msg model -> Internal msg model


toElmUpdateFn : Handler msg model -> msg -> model -> ( model, Cmd msg )
toElmUpdateFn handler msg model =
    init model |> handler msg |> toElmReturn
