module Return3 exposing
    ( Return3
    , Return3F
    , andDo
    , andDoWhen
    , andThen
    , map
    , sub
    , toElmUpdateFn
    )

import Return


type alias Return3 msg model reply =
    ( Return.Return msg model, List reply )


type alias Return3F msg model reply =
    Return3 msg model reply -> Return3 msg model reply


andDo : Cmd msg -> Return3F msg model reply
andDo cmd r3 =
    r3


andDoWhen : (model -> Bool) -> Cmd msg -> Return3F msg model reply
andDoWhen pred cmd r3 =
    r3


map : (model -> model) -> Return3F msg model reply
map fn r3 =
    r3


singleton : model -> Return3 msg model reply
singleton model =
    ( Return.singleton model, [] )


toElmUpdateFn :
    (msg -> Return3F msg model reply)
    -> msg
    -> model
    -> Return.Return msg model
toElmUpdateFn update msg model =
    update msg (singleton model)
        |> Tuple.first


sub su sm smo r3 =
    let
        _ =
            su sm smo
    in
    r3


getModel r3 =
    r3 |> Tuple.first |> Tuple.first


andThen fn r3 =
    fn (getModel r3) r3
