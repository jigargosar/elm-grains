module Worker exposing (main)

import Return


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch []


init flags =
    Return.singleton {}


update msg model =
    Return.singleton model
