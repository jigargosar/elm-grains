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

import BasicsX exposing (callWith)
import Return


type alias Return3 msg model reply =
    ( Return.Return msg model, List reply )


type alias Update3F msg model reply =
    msg -> Return3F msg model reply


type alias Return3F msg model reply =
    Return3 msg model reply -> Return3 msg model reply


andDo : Cmd msg -> Return3F msg model reply
andDo cmd r3 =
    r3


andDoWhen : (model -> Bool) -> Cmd msg -> Return3F msg model reply
andDoWhen pred cmd r3 =
    r3


map :
    (modelA -> modelB)
    -> Return3 msg modelA reply
    -> Return3 msg modelB reply
map fn r3 =
    Tuple.mapFirst (Return.map fn) r3


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


sub :
    Update3F msgS modelS replyS
    -> msgS
    -> modelS
    -> (msgS -> msg)
    -> (modelS -> model -> model)
    -> (replyS -> msg)
    -> Update3F msg model reply
    -> Return3F msg model reply
sub su sm smo toMsg set replyToMsg update r3 =
    let
        _ =
            su sm (singleton smo)
                |> mapCmd toMsg
                |> map (set >> callWith (getModel r3))
    in
    r3


mapCmd fn r3 =
    r3 |> Tuple.mapFirst (Return.mapCmd fn)


getModel : Return3 msg model reply -> model
getModel r3 =
    r3 |> Tuple.first |> Tuple.first


andThen : (model -> Return3F msg model reply) -> Return3F msg model reply
andThen fn r3 =
    fn (getModel r3) r3
