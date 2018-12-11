module Grain exposing
    ( Grain
    , decoder
    , encoder
    , generator
    , title
    , toDomIdWithPrefix
    )

import DecodeX exposing (Encoder)
import GrainId exposing (GrainId(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Random exposing (Generator)


type alias Model =
    { id : GrainId
    , title : String
    }


type Grain
    = Grain Model


init : GrainId -> Grain
init initialId =
    Grain { id = initialId, title = "" }


encoder : Encoder Grain
encoder (Grain model) =
    E.object
        [ ( "id", GrainId.encoder model.id )
        , ( "title", E.string model.title )
        ]


decoder : Decoder Grain
decoder =
    DecodeX.start Model
        |> required "id" GrainId.decoder
        |> required "title" D.string
        |> D.map Grain


unwrap (Grain model) =
    model


map fn =
    unwrap >> fn >> Grain


title =
    unwrap >> .title


id =
    unwrap >> .id


toDomIdWithPrefix prefix =
    id >> GrainId.toDomIdWithPrefix prefix


generator : Generator Grain
generator =
    GrainId.generator |> Random.map init
