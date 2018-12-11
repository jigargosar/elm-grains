module Grain exposing
    ( Grain
    , decoder
    , encoder
    , generator
    , id
    , idEq
    , setTitle
    , title
    , toDomIdWithPrefix
    )

import BasicsX exposing (eqs)
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


map : (Model -> Model) -> Grain -> Grain
map fn =
    unwrap >> fn >> Grain


title =
    unwrap >> .title


id =
    unwrap >> .id


idEq gid =
    id >> eqs gid


toDomIdWithPrefix prefix =
    id >> GrainId.toDomIdWithPrefix prefix


generator : Generator Grain
generator =
    GrainId.generator |> Random.map init


setTitle : String -> Grain -> Grain
setTitle newTitle =
    map (\model -> { model | title = newTitle })
