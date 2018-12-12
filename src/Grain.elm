module Grain exposing
    ( Grain
    , content
    , decoder
    , encoder
    , generator
    , id
    , idEq
    , setContent
    , titleOrEmpty
    , toDomIdWithPrefix
    )

import BasicsX exposing (eqs)
import DecodeX exposing (Encoder)
import GrainId exposing (GrainId(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as E
import Random exposing (Generator)


type alias Model =
    { id : GrainId
    , content : String
    }


type Grain
    = Grain Model


init : GrainId -> Grain
init initialId =
    Grain { id = initialId, content = "", revision = Revision.init }


encoder : Encoder Grain
encoder (Grain model) =
    E.object
        [ ( "id", GrainId.encoder model.id )
        , ( "content", E.string model.content )
        ]


decoder : Decoder Grain
decoder =
    DecodeX.start Model
        |> required "id" GrainId.decoder
        |> required "content" D.string
        |> D.map Grain


unwrap (Grain model) =
    model


map : (Model -> Model) -> Grain -> Grain
map fn =
    unwrap >> fn >> Grain


titleFromContent =
    String.trim
        >> String.split "\n"
        >> List.head
        >> Maybe.map String.trim


titleOrEmpty =
    content >> titleFromContent >> Maybe.withDefault ""


content =
    unwrap >> .content


id =
    unwrap >> .id


idEq gid =
    id >> eqs gid


toDomIdWithPrefix prefix =
    id >> GrainId.toDomIdWithPrefix prefix


generator : Generator Grain
generator =
    GrainId.generator |> Random.map init


setContent : String -> Grain -> Grain
setContent newContent =
    map (\model -> { model | content = newContent })
