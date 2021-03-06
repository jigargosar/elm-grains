module Return3 exposing
    ( Return3
    , Return3F
    , andThen
    , dispatch
    , do
    , doWhen
    , doWith
    , effect
    , map
    , mergeR2
    , reply
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


do : Cmd msg -> Return3F msg model reply
do cmd =
    Tuple.mapFirst (Return.command cmd)


doWhen : Pred model -> Cmd msg -> Return3F msg model reply
doWhen pred cmd =
    when (getModel >> pred) (do cmd)


doWith : (model -> a) -> (a -> Cmd msg) -> Return3F msg model reply
doWith with cmdFn r3 =
    let
        cmd =
            getModel r3 |> with |> cmdFn
    in
    do cmd r3


effect : (model -> Cmd msg) -> Return3F msg model reply
effect fn r3 =
    fn (getModel r3)
        |> do
        |> callWith r3


map :
    (modelA -> modelB)
    -> Return3 msg modelA reply
    -> Return3 msg modelB reply
map fn =
    Tuple.mapFirst (Return.map fn)


singleton : model -> Return3 msg model reply
singleton model =
    ( Return.singleton model, [] )


mergeR2 : Return.Return msg model -> Return3F msg model reply
mergeR2 ( model, cmd ) =
    map (always model) >> do cmd


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
    SubConfig msgS modelS replyS msg model reply
    -> msgS
    -> Return3F msg model reply
dispatch { toMsg, update } subMsg =
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
        >> do (Cmd.map toMsg subCmd)
        >> handleReplies


getModel : Return3 msg model reply -> model
getModel r3 =
    r3 |> Tuple.first |> Tuple.first


getReplies : Return3 msg model reply -> List reply
getReplies r3 =
    r3 |> Tuple.second


andThen : (model -> Return3F msg model reply) -> Return3F msg model reply
andThen fn r3 =
    fn (getModel r3) r3


reply rep =
    Tuple.mapSecond ((::) rep)
