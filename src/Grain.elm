module Grain exposing
    ( Grain
    , content
    , decoder
    , encoder
    , generator
    , id
    , idEq
    , idString
    , setContent
    , titleOrEmpty
    , toDomIdWithPrefix
    )

import BasicsX exposing (eqs)
import DecodeX exposing (Encoder)
import GrainId exposing (GrainId(..))
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as E
import Random exposing (Generator)
import Time exposing (Posix)
import TimeX


type alias Model =
    { id : GrainId
    , content : String
    , createdAt : Posix
    , modifiedAt : Posix
    }


type Grain
    = Grain Model


new : Posix -> GrainId -> Grain
new now newId =
    Grain
        { id = newId
        , content = ""
        , createdAt = now
        , modifiedAt = now
        }


encoder : Encoder Grain
encoder (Grain model) =
    E.object
        [ ( "id", GrainId.encoder model.id )
        , ( "content", E.string model.content )
        , ( "createdAt", TimeX.posixEncoder model.createdAt )
        , ( "modifiedAt", TimeX.posixEncoder model.modifiedAt )
        ]


decoder : Decoder Grain
decoder =
    DecodeX.start Model
        |> required "id" GrainId.decoder
        |> required "content" D.string
        |> required "createdAt" TimeX.posixDecoder
        |> required "modifiedAt" TimeX.posixDecoder
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


idString =
    id >> GrainId.toString


generator : Posix -> Generator Grain
generator now =
    Random.map (new now) GrainId.generator


setContent : String -> Grain -> Grain
setContent newContent =
    map (\model -> { model | content = newContent })
