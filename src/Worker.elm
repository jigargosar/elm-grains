port module Worker exposing (main)

import Json.Encode exposing (Value)
import Return


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


type alias Flags =
    {}


type alias Model =
    {}


init flags =
    Return.singleton {}


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
