module HistoryState exposing
    ( HistoryState
    , decoder
    , encoder
    , focusedDomId
    , init
    )

import Json.Decode as D
import Json.Decode.Pipeline exposing (optional)
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
    D.succeed HistoryState
        |> optional "focusedDomId" (D.maybe D.string) Nothing


focusedDomId =
    .focusedDomId
