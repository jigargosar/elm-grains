port module Worker exposing (main)

import Json.Encode exposing (Value)
import Port
import Return


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


type alias Flags =
    {}


type alias Model =
    {}


init flags =
    ( {}, Port.error "foo" )


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
