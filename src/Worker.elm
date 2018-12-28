port module Worker exposing (main)

import Json.Encode exposing (Value)
import Port
import Return
import Time exposing (Posix)


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


type alias Flags =
    { now : Int }


type alias Model =
    {}


init flags =
    let
        _ =
            Debug.log "posix" (Time.millisToPosix flags.now)
    in
    ( {}
    , Port.error <|
        "foo: "
            ++ String.fromInt flags.now
    )


subscriptions model =
    Sub.batch []


type Msg
    = NoOp


update msg model =
    Return.singleton model


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
