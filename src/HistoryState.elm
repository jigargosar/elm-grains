module HistoryState exposing (HistoryState, decoder, encoder, init)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe


type alias HistoryState =
    { focusedDomId : Maybe String
    }


init =
    HistoryState


encoder model =
    E.object
        [ ( "focusedDomId", Maybe.unwrap E.null E.string model.focusedDomId )
        ]


decoder =
    D.map HistoryState
        (D.maybe D.string)
