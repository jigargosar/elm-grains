module Return3 exposing (Return2, Return3, Return3F, andDo, andDoWhen, map, toElmUpdateFn)

import Return


type alias Return2 msg model =
    Return.Return msg model


type alias Return3 msg model reply =
    ( Return2 msg model, List reply )


type alias Return3F msg model reply =
    Return3 msg model reply -> Return3 msg model reply


andDo cmd r3 =
    r3


andDoWhen pred cmd r3 =
    r3


map fn r3 =
    r3


singleton model =
    ( Return.singleton model, [] )


toElmUpdateFn update msg model =
    update msg (singleton model)
        |> Tuple.first
