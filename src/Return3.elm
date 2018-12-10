module Return3 exposing
    ( Return3
    , Return3F
    , andDo
    , andDoWhen
    , andThen
    , dispatch
    , map
    , sub
    , toElmUpdate
    )

import BasicsX exposing (Pred, callWith, when)
import Return


type alias Return3 msg model reply =
    ( Return.Return msg model, List reply )


type alias Update3F msg model reply =
    msg -> Return3F msg model reply


type alias Return3F msg model reply =
    Return3 msg model reply -> Return3 msg model reply


andDo : Cmd msg -> Return3F msg model reply
andDo cmd =
    Tuple.mapFirst (Return.command cmd)


andDoWhen : Pred model -> Cmd msg -> Return3F msg model reply
andDoWhen pred cmd =
    when (getModel >> pred) (andDo cmd)


map :
    (modelA -> modelB)
    -> Return3 msg modelA reply
    -> Return3 msg modelB reply
map fn =
    Tuple.mapFirst (Return.map fn)


singleton : model -> Return3 msg model reply
singleton model =
    ( Return.singleton model, [] )


toElmUpdate :
    Update3F msg model reply
    -> msg
    -> model
    -> Return.Return msg model
toElmUpdate update msg model =
    update msg (singleton model)
        |> Tuple.first


type alias SubConfig msgS modelS replyS msg model reply =
    { subUpdate : Update3F msgS modelS replyS
    , get : model -> modelS
    , set : modelS -> model -> model
    , toMsg : msgS -> msg
    , replyToMsg : replyS -> msg
    , update : Update3F msg model reply
    }


dispatch :
    msgS
    -> SubConfig msgS modelS replyS msg model reply
    -> Return3F msg model reply
dispatch subMsg { toMsg, update } =
    update (toMsg subMsg)


sub :
    msgS
    -> SubConfig msgS modelS replyS msg model reply
    -> Return3F msg model reply
sub subMsg { subUpdate, get, toMsg, set, replyToMsg, update } r3 =
    let
        subModel =
            get (getModel r3)

        ( ( newSubModel, subCmd ), replies ) =
            subUpdate subMsg (singleton subModel)

        handleReplies newR3 =
            List.foldl (replyToMsg >> update) newR3 replies
    in
    r3
        |> map (set newSubModel)
        >> andDo (Cmd.map toMsg subCmd)
        >> handleReplies


mapCmd fn r3 =
    r3 |> Tuple.mapFirst (Return.mapCmd fn)


getModel : Return3 msg model reply -> model
getModel r3 =
    r3 |> Tuple.first |> Tuple.first


getReplies : Return3 msg model reply -> List reply
getReplies r3 =
    r3 |> Tuple.second


andThen : (model -> Return3F msg model reply) -> Return3F msg model reply
andThen fn r3 =
    fn (getModel r3) r3
